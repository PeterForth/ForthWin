# $Id: Makefile,v 1.2 2008/06/27 13:55:17 ygreks Exp $

target: linux.const

%.auto.f: %.gen
		./$< > $@

consts.gen: consts.c
		gcc -Wall -Werror $< -o$@

linux.const: consts.auto.f compile.f
		spf compile.f

clean:
		rm -f consts.gen consts.auto.f
