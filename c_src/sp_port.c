#define _XOPEN_SOURCE 500

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <libserialport.h>

#include "erlcmd.h"

#define CHECK_EI(x) \
    do { \
        if (x < 0) \
            errx(EXIT_FAILURE, #x " failed at %s:%d", __FILE__, __LINE__); \
    } while (0)

#define CHECK_SP(x) \
    do { \
        if (x != SP_OK) \
            errx(EXIT_FAILURE, #x ": %s", sp_last_error_message()); \
    } while (0)

#define SP_BUF_SIZE     1024
#define WRITE_TIMEOUT   500

#define REPLY           0
#define NOTIF           1

struct sp
{
    struct sp_port *port;
    int fd;
    char buffer[SP_BUF_SIZE];
};

static void sp_init(struct sp *sp, const char *portname,
        int baudrate, int flowcontrol)
{
    struct sp_port_config *config = NULL;

    CHECK_SP(sp_get_port_by_name(portname, &sp->port));
    CHECK_SP(sp_open(sp->port, SP_MODE_READ_WRITE));
    CHECK_SP(sp_get_port_handle(sp->port, &sp->fd));
    CHECK_SP(sp_flush(sp->port, SP_BUF_BOTH));

    CHECK_SP(sp_new_config(&config));
    CHECK_SP(sp_get_config(sp->port, config));
    CHECK_SP(sp_set_config_baudrate(config, baudrate));
    CHECK_SP(sp_set_config_bits(config, 8));
    CHECK_SP(sp_set_config_parity(config, SP_PARITY_NONE));
    CHECK_SP(sp_set_config_stopbits(config, 1));
    CHECK_SP(sp_set_config_flowcontrol(config, flowcontrol));
    CHECK_SP(sp_set_config_dtr(config, SP_DTR_ON));
    CHECK_SP(sp_set_config(sp->port, config));

    if (config != NULL)
        sp_free_config(config);

    memset(sp->buffer, 0, sizeof(sp->buffer));
}

static void init_resp(ei_x_buff *resp, int notif)
{
    char hdr[3] = {0, 0, notif != 0};

    CHECK_EI(ei_x_new(resp));
    CHECK_EI(ei_x_append_buf(resp, hdr, sizeof(hdr)));
    CHECK_EI(ei_x_encode_version(resp));
}

static void sp_process(struct sp *sp)
{
    ei_x_buff resp;
    int res;

    init_resp(&resp, NOTIF);

    CHECK_EI(ei_x_encode_tuple_header(&resp, 2));

    res = sp_nonblocking_read(sp->port, sp->buffer, sizeof(sp->buffer));
    if (res < 0) {
        CHECK_EI(ei_x_encode_atom(&resp, "sp_error"));
        CHECK_EI(ei_x_encode_long(&resp, sp_last_error_code()));
    } else {
        CHECK_EI(ei_x_encode_atom(&resp, "sp_data"));
        CHECK_EI(ei_x_encode_binary(&resp, sp->buffer, res));
    }

    erlcmd_send(resp.buff, resp.index);

    CHECK_EI(ei_x_free(&resp));
}

static void sp_handle_request(const char *req, void *cookie)
{
    struct sp *sp = (struct sp *)cookie;
    int req_index = sizeof(uint16_t);
    char cmd[MAXATOMLEN];
    int arity;
    ei_x_buff resp;

    if (ei_decode_version(req, &req_index, NULL) < 0)
        errx(EXIT_FAILURE, "message version issue?");

    if (ei_decode_tuple_header(req, &req_index, &arity) < 0 || arity != 2)
        errx(EXIT_FAILURE, "expecting {cmd, args} tuple");

    if (ei_decode_atom(req, &req_index, cmd) < 0)
        errx(EXIT_FAILURE, "expecting command atom");

    init_resp(&resp, REPLY);

    if (strcmp(cmd, "write") == 0) {
        int type, size;
        long res, len;

        CHECK_EI(ei_get_type(req, &req_index, &type, &size));
        if ((size_t)size > sizeof(sp->buffer))
            errx(EXIT_FAILURE, "binary too large");

        CHECK_EI(ei_decode_binary(req, &req_index, sp->buffer, &len));

        res = sp_blocking_write(sp->port, sp->buffer, len, WRITE_TIMEOUT);
        if (res < 0) {
            CHECK_EI(ei_x_encode_tuple_header(&resp, 2));
            CHECK_EI(ei_x_encode_atom(&resp, "error"));
            CHECK_EI(ei_x_encode_long(&resp, sp_last_error_code()));
        } else if (res != len) {
            CHECK_EI(ei_x_encode_tuple_header(&resp, 2));
            CHECK_EI(ei_x_encode_atom(&resp, "error"));
            CHECK_EI(ei_x_encode_atom(&resp, "timeout"));
        } else {
            CHECK_EI(ei_x_encode_atom(&resp, "ok"));
        }
    } else {
        errx(EXIT_FAILURE, "unknown command: %s", cmd);
    }

    erlcmd_send(resp.buff, resp.index);

    CHECK_EI(ei_x_free(&resp));
}

static void sp_list_handle_request(const char *req,
        void *cookie __attribute__ ((unused)))
{
    int req_index = sizeof(uint16_t);
    char cmd[MAXATOMLEN];

    if (ei_decode_version(req, &req_index, NULL) < 0)
        errx(EXIT_FAILURE, "message version issue?");

    if (ei_decode_atom(req, &req_index, cmd) < 0)
        errx(EXIT_FAILURE, "expecting command atom");

    if (strcmp(cmd, "list") == 0) {
        ei_x_buff resp;
        struct sp_port **ports, **port;

        init_resp(&resp, REPLY);

        CHECK_SP(sp_list_ports(&ports));

        for (port = ports; *port != NULL; port++) {
            CHECK_EI(ei_x_encode_list_header(&resp, 1));
            CHECK_EI(ei_x_encode_string(&resp, sp_get_port_name(*port)));
        }
        CHECK_EI(ei_x_encode_empty_list(&resp));

        erlcmd_send(resp.buff, resp.index);

        sp_free_port_list(ports);
        CHECK_EI(ei_x_free(&resp));
    } else {
        errx(EXIT_FAILURE, "unknown command: %s", cmd);
    }
}

static void usage(char *argv[])
{
    errx(EXIT_FAILURE,
            "%s sp list | <portname> <baudrate> <flowcontrol>", argv[0]);
}

int sp_main(int argc, char *argv[])
{
    struct erlcmd handler;

    if (argc < 3)
        usage(argv);

    if (strcmp(argv[2], "list") == 0) {
        if (argc != 3)
            usage(argv);

        erlcmd_init(&handler, sp_list_handle_request, NULL);

        for (;;) {
            erlcmd_process(&handler);
        }
    } else {
        char *portname;
        int baudrate = 0;
        int flowcontrol = 0;
        struct sp sp;

        if (argc != 5)
            usage(argv);

        portname = argv[2];

        if (sscanf(argv[3], "%d", &baudrate) != 1)
            usage(argv);

        if (sscanf(argv[4], "%d", &flowcontrol) != 1)
            usage(argv);

        sp_init(&sp, portname, baudrate, flowcontrol);
        erlcmd_init(&handler, sp_handle_request, &sp);

        for (;;) {
            int ret;
            struct pollfd fds[2];

            fds[0].fd = STDIN_FILENO;
            fds[0].events = POLLIN;
            fds[0].revents = 0;

            fds[1].fd = sp.fd;
            fds[1].events = POLLIN;
            fds[1].revents = 0;

            ret = poll(fds, 2, -1);
            if (ret < 0) {
                if (errno == EINTR)
                    continue;
                err(EXIT_FAILURE, "poll");
            }

            if (fds[0].revents & (POLLIN | POLLHUP))
                erlcmd_process(&handler);

            if (fds[1].revents & POLLIN)
                sp_process(&sp);
        }
    }

    return 0;
}
