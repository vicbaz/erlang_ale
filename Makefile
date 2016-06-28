PROJECT = erlang_ale
PROJECT_DESCRIPTION = Erlang/ALE - Erlang Actor Library for Embedded
PROJECT_VERSION = 0.1.0

C_SRC_TYPE = executable
CFLAGS = -O3 -std=c99 -finline-functions -Wall -Wextra -Werror -Wno-pointer-to-int-cast

include erlang.mk

LDLIBS += -lserialport
