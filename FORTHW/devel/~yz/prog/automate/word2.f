\ �. �������, http://www.forth.org.ru/~yz
\ ������ ������������� ���������� Automate � ������ �������������

REQUIRE [[ ~yz/lib/automate.f

0 VALUE word
0 VALUE content

" Hello\n" ASCIIZ str

ComInit DROP

  " Word.Application" ?CreateObject
  .( ?CreateObject= ) . CR
  TO word
  word [[ Visible =  TRUE ]]
  word [[ Documents Add ]] release
  word [[ ActiveDocument Content ]] TO content
  content [[ InsertAfter ( " � ��������," ) ]]
  500 PAUSE
  content [[ InsertParagraphAfter ]]
  content [[ { S" InsertAfter" } ( " ��� ���� ���������" ) ]]
  500 PAUSE
  content [[ InsertParagraphAfter ]]
  content [[ InsertAfter ( " ����������� ����..." ) ]]
  500 PAUSE
  content [[ Font Size = 20 ]]
  800 PAUSE
  word [[ ActiveDocument SaveAs ( " c:/����������� ����.doc" ) * ]]
  content release
  word [[ Quit ]]
  word release

ComDestroy
BYE
