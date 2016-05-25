#define _XOPEN_SOURCE 500

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <libserialport.h>

#include "erlcmd.h"

struct sp
{
    struct sp_port *port;
    int fd;
};

static void sp_init(struct sp *sp, const char *portname)
{
    struct sp_port_config *config = NULL;

    if (sp_get_port_by_name(portname, &sp->port) != SP_OK)
        errx(EXIT_FAILURE, "sp_get_port_by_name: %s", sp_last_error_message());

    if (sp_open(sp->port, SP_MODE_READ_WRITE) != SP_OK)
        errx(EXIT_FAILURE, "sp_open: %s", sp_last_error_message());

    if (sp_get_port_handle(sp->port, &sp->fd) != SP_OK)
        errx(EXIT_FAILURE, "sp_get_port_handle: %s", sp_last_error_message());

    if (sp_flush(sp->port, SP_BUF_BOTH) != SP_OK)
        errx(EXIT_FAILURE, "sp_flush: %s", sp_last_error_message());

    if (sp_new_config(&config) != SP_OK)
        errx(EXIT_FAILURE, "sp_new_config: %s", sp_last_error_message());
    else if (sp_get_config(sp->port, config) != SP_OK)
        errx(EXIT_FAILURE, "sp_get_config: %s", sp_last_error_message());
    else if (sp_set_config_baudrate(config, 115200) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_baudrate: %s", sp_last_error_message());
    else if (sp_set_config_bits(config, 8) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_bits: %s", sp_last_error_message());
    else if (sp_set_config_parity(config, SP_PARITY_NONE) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_parity: %s", sp_last_error_message());
    else if (sp_set_config_stopbits(config, 1) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_stopbits: %s", sp_last_error_message());
    else if (sp_set_config_dtr(config, SP_DTR_ON) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_dtr: %s", sp_last_error_message());
    else if (sp_set_config_xon_xoff(config, SP_XONXOFF_DISABLED) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_xon_xoff: %s", sp_last_error_message());
    else if (sp_set_config_flowcontrol(config, SP_FLOWCONTROL_NONE) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config_flowcontrol: %s", sp_last_error_message());
    else if (sp_set_config(sp->port, config) != SP_OK)
        errx(EXIT_FAILURE, "sp_set_config: %s", sp_last_error_message());

    if (config != NULL)
        sp_free_config(config);
}

static void sp_process(struct sp *sp)
{
}

static void sp_handle_request(const char *req, void *cookie)
{
    struct sp *sp = (struct sp *)cookie;
}

int sp_main(int argc, char *argv[])
{
    struct sp sp;
    struct erlcmd handler;

    if (argc != 3)
        errx(EXIT_FAILURE, "%s sp <port name>", argv[0]);

    sp_init(&sp, argv[2]);
    erlcmd_init(&handler, sp_handle_request, &sp);

    for (;;) {
        int rc;
        struct pollfd fdset[2];

        fdset[0].fd = STDIN_FILENO;
        fdset[0].events = POLLIN;
        fdset[0].revents = 0;

        fdset[1].fd = sp.fd;
        fdset[1].events = POLLPRI;
        fdset[1].revents = 0;

        rc = poll(fdset, 2, -1);
        if (rc < 0) {
            if (errno == EINTR)
                continue;
            err(EXIT_FAILURE, "poll");
        }

        if (fdset[0].revents & (POLLIN | POLLHUP))
            erlcmd_process(&handler);

        if (fdset[1].revents & POLLPRI)
            sp_process(&sp);
    }

    return 0;
}
