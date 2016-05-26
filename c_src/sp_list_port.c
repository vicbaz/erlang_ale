#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libserialport.h>

#include "erlcmd.h"

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
        char hdr[2] = {0, 0};
        struct sp_port **ports, **port;

        CHECK(ei_x_new(&resp));
        CHECK(ei_x_append_buf(&resp, hdr, sizeof(hdr)));
        CHECK(ei_x_encode_version(&resp));

        if (sp_list_ports(&ports) != SP_OK)
            errx(EXIT_FAILURE, "sp_list_ports: %s", sp_last_error_message());

        for (port = ports; *port != NULL; port++) {
            CHECK(ei_x_encode_list_header(&resp, 1));
            CHECK(ei_x_encode_string(&resp, sp_get_port_name(*port)));
        }
        CHECK(ei_x_encode_empty_list(&resp));

        erlcmd_send(resp.buff, resp.index);

        sp_free_port_list(ports);
        CHECK(ei_x_free(&resp));
    } else {
        errx(EXIT_FAILURE, "unknown command: %s", cmd);
    }
}

int sp_list_main()
{
    struct erlcmd handler;
    erlcmd_init(&handler, sp_list_handle_request, NULL);

    for (;;)
        erlcmd_process(&handler);

    return 1;
}
