/*
 *  Copyright 2015 Frank Hunleth
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int gpio_main(int argc, char *argv[]);
extern int i2c_main(int argc, char *argv[]);
extern int spi_main(int argc, char *argv[]);
extern int sp_main(int argc, char *argv[]);

int main(int argc, char *argv[])
{
    if (argc < 2)
        errx(EXIT_FAILURE, "must pass mode (e.g. gpio, i2c, spi, sp)");

    if (strcmp(argv[1], "gpio") == 0)
        return gpio_main(argc, argv);
    else if (strcmp(argv[1], "i2c") == 0)
        return i2c_main(argc, argv);
    else if (strcmp(argv[1], "spi") == 0)
        return spi_main(argc, argv);
    else if (strcmp(argv[1], "sp") == 0)
        return sp_main(argc, argv);
    else
        errx(EXIT_FAILURE, "unknown mode '%s'", argv[1]);

    return 1;
}
