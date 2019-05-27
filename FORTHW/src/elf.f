\ ����� ����� spf4.o (������ ELF)
\ �. �������, 4.05.2007

\ ������ �����:

\ ���������

\ ������� ������:
\   0. ������� ������
\   1. ������� ���� ������
\   2. ������� ���� 
\   3. ������� ��������
\   4. ������� �����������
\   5. ����-�������
\   6. ������ ����� ��� ���������� �������
\   7. ������� ������� ������� �������
\   8. ������� ����� ��� ������� ������� �������

\ ������� ���������:

\ ������ =============================

CREATE .shstrtab
0 C,
ASCIIZ" .shstrtab"
ASCIIZ" .strtab"
ASCIIZ" .symtab"
ASCIIZ" .rel.forth"
ASCIIZ" .space"
ASCIIZ" .dltable"
ASCIIZ" .dlstrings"
 
HERE .shstrtab - CONSTANT .shstrtab#

\ ----------------------------------- 

CREATE .strtab
0 C,
ASCIIZ" main"
ASCIIZ" dlopen"
ASCIIZ" dlsym"
ASCIIZ" realloc"
ASCIIZ" write"
ASCIIZ" calloc"
ASCIIZ" dlerror"
 
HERE .strtab - CONSTANT .strtab#

\ ----------------------------------- 

CREATE .symtab

\ ------------------------------------

\ #0 ������ ������ - �������
0 ,  \ ���
0 ,  \ �����
0 ,  \ ������
0 C, \ ����������
0 C, \ ������
0 W, \ ������

\ #1 ������ forth
0 ,  \ ���
0 ,  \ �����
0 ,  \ ������
3 C, \ local+section
0 C, \ ������
5 W, \ ������

\ #2 ������ space
0 ,  \ ���
0 ,  \ �����
0 ,  \ ������
3 C, \ local+section
0 C, \ ������
6 W, \ ������

\ #3 ������ .dltable
0 ,  \ ���
0 ,  \ �����
0 ,  \ ������
3 C, \ local+section
0 C, \ ������
7 W, \ ������

\ #4 ������ .dlstrings
0 ,  \ ���
0 ,  \ �����
0 ,  \ ������
3 C, \ local+section
0 C, \ ������
8 W, \ ������

\ #5 ������� ������� main
1 ,          \ ���
'' INIT .forth - ,   \ �����
30 ,         \ ������ !!!
18 C,        \ global+func
0 C,         \ ������
5 W,         \ ������

\ #6 ������� ������� dlopen
6 ,   \ ���
0 ,   \ �����
0 ,   \ ������
16 C, \ global+func
0 C,  \ ������
0 W,  \ ������

\ #7 ������� ������� dlsym
13 ,  \ ���
0 ,   \ �����
0 ,   \ ������
16 C, \ global+func
0 C,  \ ������
0 W,  \ ������

\ #8 ������� ������� realloc
19 ,  \ ���
0 ,   \ �����
0 ,   \ ������
16 C, \ global+func
0 C,  \ ������
0 W,  \ ������

\ #9 ������� ������� write
27 ,  \ ���
0 ,   \ �����
0 ,   \ ������
16 C, \ global+func
0 C,  \ ������
0 W,  \ ������

\ #9 ������� ������� calloc
33 ,  \ ���
0 ,   \ �����
0 ,   \ ������
16 C, \ global+func
0 C,  \ ������
0 W,  \ ������

\ #10 ������� ������� calloc
40 ,  \ ���
0 ,   \ �����
0 ,   \ ������
16 C, \ global+func
0 C,  \ ������
0 W,  \ ������

HERE .symtab - CONSTANT .symtab#

\ ------------------------------------

CREATE .rel.forth

\ ������ .dltable
'' dl-first 5 + .forth - ,  \ �����
3 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ .dlstrings
'' dl-first-strtab 5 + .forth - ,  \ �����
4 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ dlopen
[T] dlopen-adr [I] .forth - , \ �����
6 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ dlsym
[T] dlsym-adr [I] .forth - ,  \ �����
7 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ realloc
[T] realloc-adr [I] .forth - ,  \ �����
8 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ write
[T] write-adr [I] .forth - ,  \ �����
9 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ calloc
[T] calloc-adr [I] .forth - ,  \ �����
10 8 LSHIFT 1 OR , \ ��� r386_32

\ ������ dlerror
[T] dlerror-adr [I] .forth - ,  \ �����
11 8 LSHIFT 1 OR , \ ��� r386_32

HERE .rel.forth - CONSTANT .rel.forth#

dl-second-strtab @ CONSTANT .dlstrings#
dl-second# dl-rec# * CONSTANT .dltable#

dl-second# '' dl-first# 5 + !

\ ====================================
 
CREATE sections
\ ������ 0: �������
0 ,    \ ���
0 ,    \ ���
0 ,    \ �����
0 ,    \ �����
0 ,    \ ��������
0 ,    \ ������
0 ,    \ ������
0 ,    \ �������������� ����������
0 ,    \ ������������
0 ,    \ ������ ������
 
 \ ������ 1: ������� ���� ������
1 ,    \ ��� .shstrtab
3 ,    \ ��� = sht_strtab
0 ,    \ �����
0 ,    \ �����
\ �������� � ������
.shstrtab# offset,size,
0 ,    \ ������
0 ,    \ �������������� ����������
1 ,    \ ������������
0 ,    \ ������ ������
 
\ ������ 2: .strtab
11 ,   \ ��� .strtab
3 ,    \ ��� = sht_strtab
0 ,    \ �����
0 ,    \ �����
\ �������� � ������
.strtab# offset,size,
0 ,    \ ������
0 ,    \ �������������� ����������
1 ,    \ ������������
0 ,    \ ������ ������

\ ������ 3: .symtab
19 ,   \ ��� .symtab
2 ,    \ ��� = sht_symtab
0 ,    \ �����
0 ,    \ �����
\ �������� � ������
.symtab# offset,size,
2 ,    \ ������� ���� � ������ 2
5 ,    \ ��������� ��������
4 ,    \ ������������
symbol-size ,    \ ������ ������

\ ������ 4: .rel.forth
27 ,   \ ��� .symtab
9 ,    \ ��� = sht_rel
0 ,    \ �����
0 ,    \ �����
\ �������� � ������
.rel.forth# offset,size,
3 ,    \ ���������� ������� � ������ 3
5 ,    \ ����������� ��� ������ 5 .forth
4 ,    \ ������������
rel-size ,    \ ������ ������

offset TO .forth-offset

\ ������ 5: .forth
31 ,            \ ��� .forth
1 ,             \ ��� = sht_progbits
0x7 ,           \ �����: shf_write+shf_alloc+shf_exec
0 ,             \ �����
\ �������� � ������
.forth# offset,size,
0 ,             \ ����������
0 ,             \ ��������� ��������
4 ,             \ ������������
0 ,             \ ������ ������

\ ������ 6: .space
38 ,            \ ��� .space
8 ,             \ ��� = sht_nobits
0x7 ,           \ �����: shf_write+shf_alloc+shf_exec
0 ,             \ �����
\ �������� � ������
offset ,
IMAGE-SIZE ,
0 ,             \ ����������
0 ,             \ ��������� ��������
4 ,             \ ������������
0 ,             \ ������ ������

\ ������ 7: .dltable
45 ,            \ ��� .dltable
1 ,             \ ��� = sht_progbits
0x3 ,           \ �����: shf_write+shf_alloc
0 ,             \ �����
\ �������� � ������
.dltable# offset,size,
0 ,             \ ����������
0 ,             \ ��������� ��������
4 ,             \ ������������
0 ,             \ ������ ������

\ ������ 6: .dlstrings
54 ,            \ ��� .dlstrings
3 ,             \ ��� = sht_startab
0x2 ,           \ �����: shf_alloc
0 ,             \ �����
\ �������� � ������
.dlstrings# offset,size,
0 ,             \ ����������
0 ,             \ ��������� ��������
4 ,             \ ������������
0 ,             \ ������ ������

HERE sections - CONSTANT total-sections-size
 
total-sections-size section-size / CONSTANT sections#

CREATE segments
( \ ������� 0: �������
0 ,       \ ���
0 ,       \ ��������
0 ,       \ ����������� �����
0 ,       \ ���������� �����
0 ,       \ ������ � �����
0 ,       \ ������ � ������
0 ,       \ �����
0 ,       \ ������������

\ ������� 1: .forth
1 ,               \ ���: pt_load
.forth-offset ,   \ �������� � �����
IMAGE-START ,     \ ����������� �����
0 ,               \ ���������� �����
.forth# ,         \ ������ � �����
IMAGE-SIZE ,      \ ������ � ������
7 ,               \ �����: pf_x pf_r pf_w
0 ,               \ ������������
)
 
HERE segments - CONSTANT total-segments-size
 
total-segments-size segment-size / CONSTANT segments#
 
header-size total-sections-size + total-segments-size + CONSTANT data-offset

CREATE elf-header
0x7F C, CHAR E C, CHAR L C, CHAR F C,   \ ������� ELF
1 C,          \ elfclass32
1 C,          \ elfdata2lsb
1 C,          \ elfversion = ev_current
9 ALLOT       \ padding
1 W,          \ et_rel (��������� ����)
3 W,          \ em_386 (��� ������)
1 ,           \ ev_current (������� ������)
0 ,           \ ����� �����
\ �������� ������� ���������
0 ,
\ �������� ������� ������
header-size  ,
0 ,     \ �����
header-size  W,  \ ������ ���������
segment-size W,  \ ������ ������ ������� ���������
segments#    W,  \ ����� ������� � ������� ���������
section-size W,  \ ������ ������ ������� ������
sections#    W,  \ ����� ������� � ������� ������
1	     W,  \ ����� ������ ������� �����