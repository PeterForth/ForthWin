# $Id: Makefile,v 1.3 2008/06/27 12:27:57 ygreks Exp $
#
# generate platform-specific config file

config.auto.f: config.gen
		./$< > $@

config.gen: config.c
		gcc -Wall -Werror -DSPF_SRC $< -o$@

clean:
		rm -f config.gen config.auto.f
