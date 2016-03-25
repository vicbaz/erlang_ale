#include <stdio.h>
#include <err.h>
#include <libserialport.h>

static void list_serial_ports()
{
    struct sp_port **ports, **p;

    if (sp_list_ports(&ports) != SP_OK)
        errx(1, sp_last_error_message());

    for (p = ports; *p != NULL; p++)
        printf("port: %s\n", sp_get_port_name(*p));

    sp_free_port_list(ports);
}

int main()
{
    list_serial_ports();
    return 0;
}
