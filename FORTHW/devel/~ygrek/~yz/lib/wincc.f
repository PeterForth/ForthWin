\ WINLIB 1.14.1

\ $Id: wincc.f,v 1.4 2006/11/12 07:50:01 ygreks Exp $

\ ���������� ����������������� ���������� Windows
\ �. 3. ����� �������� ����������
\ �. �������, 10.09.2002

REQUIRE WINDOWS... ~ygrek/~yz/lib/winctl.f

\ -----------------------------
\ ���������

: progress-get-pos ( ctl -- )  W: pbm_deltapos ?send ;
: progress-set-pos ( n ctl -- ) W: pbm_setpos wsend DROP ;

: progress ( -- ctl )
  scrolllike " msctls_progress32" 0 create-control >R
  ['] progress-get-pos ['] progress-set-pos -pos R@ setitem
  R> ;

: +pos! ( n ctl -- ) W: pbm_deltapos wsend DROP ;

: progress-minmax ( min max ctl -- ) >R 16 LSHIFT OR 0 SWAP
  W: pbm_setrange R> send DROP ;

\ -----------------------------------------
\ ��������

: tbget-pos ( ctl -- pos) W: tbm_getpos ?send ;
: tbset-pos ( pos ctl -- ) >R TRUE SWAP W: tbm_setpos R> send DROP ;
: tbget-min ( ctl -- min) W: tbm_getrangemin ?send ;
: tbset-min ( min ctl -- ) >R TRUE SWAP W: tbm_setrangemin R> send DROP ;
: tbget-max ( ctl -- max) W: tbm_getrangemax ?send ;
: tbset-max ( max ctl -- ) >R TRUE SWAP W: tbm_setrangemax R> send DROP ;

: trackbar ( style -- ctl )
  scrolllike " msctls_trackbar32" ROT W: tbs_fixedlength OR create-control >R
  ['] tbget-pos ['] tbset-pos -pos R@ setitem
  ['] tbget-min ['] tbset-min -min R@ setitem
  ['] tbget-max ['] tbset-max -max R@ setitem
  0 R@ -min!
  100 R@ -max!
  R> ;

\ --------------------------------------------
\ �����

" msctls_updown32" ASCIIZ updowns

: udget-pos ( ctl -- pos) W: udm_getpos ?send LOWORD ;
: udset-pos ( pos ctl -- ) >R 0 SWAP W: udm_setpos R> send DROP ;

: udget-min ( ctl -- min) W: udm_getrange ?send HIWORD ;
: udset-min ( min ctl -- ) >R 16 LSHIFT R@ -max@ OR 0 SWAP
  W: udm_setrange R> send DROP ;

: udget-max ( ctl -- max) W: udm_getrange ?send LOWORD ;
: udset-max ( max ctl -- ) >R R@ -min@ 16 LSHIFT OR 0 SWAP
  W: udm_setrange R> send DROP ;

: updown ( style -- ctl )
  scrolllike updowns ROT create-control >R
  ['] udget-pos ['] udset-pos -pos R@ setitem
  ['] udget-min ['] udset-min -min R@ setitem
  ['] udget-max ['] udset-max -max R@ setitem
  0 R@ -pos!
  0 R@ -min!
  100 R@ -max!
  R> ;

: add-updown ( style ctl -- )
  SWAP (* uds_alignright uds_arrowkeys *) OR updown 2DUP
  >R -hwnd@ 0 W: udm_setbuddy R> send DROP
  SWAP -updown! ;

\ --------------------------------
\ ��������

: start-anime ( ctl -- )
  >R -1 0xFFFF0000 W: acm_play R> send DROP ;
: stop-anime ( ctl -- )
 W: acm_stop ?send DROP ;

: anime ( z/n -- ctl)
  control " SysAnimate32" 0 create-control >R
  transparent R@ -bgcolor!
  0 SWAP W: acm_opena R@ send DROP
  R@ start-anime 
  R> ;

\ -------------------------------
\ ���������

: adjust-calendar { ctl \ [ 4 CELLS ] rect -- }
  0 rect W: mcm_getminreqrect ctl send DROP
  rect 2 CELLS@  0 0 W: mcm_getmaxtodaywidth ctl send MAX
  rect 3 CELLS@ ctl resize ;

: get-cal-color ( i ctl -- )
  >R 0 W: mcm_getcolor R> send >bgr ;
: set-cal-color ( color i ctl -- )
  >R SWAP >bgr W: mcm_setcolor R> send DROP ;

: get-cal-bgcolor ( ctl -- )
  W: mcsc_monthbk SWAP get-cal-color ;
: set-cal-bgcolor ( color ctl -- )
  W: mcsc_monthbk SWAP set-cal-color ;

: set-calendar-font ( font ctl -- )
  >R 1 W: wm_setfont R@ send DROP
  R> adjust-calendar ;

: get-calendar-select ( a ctl -- )
  >R 0 SWAP W: mcm_getcursel R> send DROP ;

: set-calendar-select ( a ctl -- )
  >R 0 SWAP W: mcm_setcursel R> send DROP ;

: calendar ( style -- ctl )
  W: icc_date_classes initcc
  listlike " SysMonthCal32" ROT W: ws_tabstop OR create-control >R
  R@ adjust-calendar
  ['] get-cal-color ['] set-cal-color getset-flag -color R@ setflagitem
  ['] get-cal-bgcolor ['] set-cal-bgcolor getset-flag -bgcolor R@ setflagitem
  R@ -bgbrush@ DeleteObject DROP
  ['] set-calendar-font -font R@ storeset
  ['] get-calendar-select ['] set-calendar-select -selected R@ setitem
  W: mcn_select R@ -defcommand!
  R> ;

\ ---------------------------------------
\ IP-�����

\ �� �������� :-(
\ : ip-address ( -- ctl )
\  W: icc_internet_classes initcc
\  listlike " SysIPAddress32" 0 create-control >R
\  W: ipn_fieldchanged R@ -defcommand!
\  R> ;

\ ----------------------------------------------
\ ����������

listlike table container
  item -addproc		\ �������� �������
  item -delproc		\ ������� �������
  item -numproc		\ ����� ���������
  item -imagelist getset \ ������ ��������
  item -iwidth	getset  \ ������ ��������
  item -iheight	getset	\ ������ ��������
  item -iflags	getset	\ ����� ��������
  item -itext	getset	\ ����� ��������
  item -iimage	getset	\ �������� ��������
  item -iparam	getset	\ �������� ��������
endtable

: add-item ( ... i ctl -- ) DUP -addproc@ EXECUTE ;
: delete-item ( ... i ctl -- ) DUP -delproc@ EXECUTE ;
: item-count ( ... ctl -- ) DUP -numproc@ EXECUTE ;

: alloc-item ( -- a) 14 CELLS GETMEM ;

: itemsend { i ctl mask offset offset2 msg buf item getproc setproc -- n/ }
  mask item !
  setproc IF offset offset2 item setproc EXECUTE THEN
  i buf msg ctl send DROP
  getproc IF offset offset2 item getproc EXECUTE THEN
  buf FREEMEM ;

PROC: (getn) ( off1 off2 item -- n ) PRESS SWAP CELLS+ @ PROC;
: (fromitem) ( i ctl mask off off2 msg buf item -- n )
  (getn) 0 itemsend ;

PROC: (setn) ( n off1 off2 item -- ) PRESS SWAP CELLS+ ! PROC;
: (toitem) ( n i ctl mask off off2 msg buf item -- )
  0 (setn) itemsend ;

PROC: (getc) ( off1 off2 item -- c ) PRESS + C@ PROC;
: (bytefromitem) ( i ctl mask off off2 msg buf item -- c)
  \ ����� off1 � ������, � �� � �������
  (getc) 0 itemsend ;

PROC: (setc) ( c off1 off2 item -- ) PRESS + C! PROC;
: (bytetoitem) ( c ctl mask off off2 msg buf item -- )
  \ ����� off1 � ������, � �� � �������
  0 (setc) itemsend ;

PROC: (gettext) ( z off1 off2 item -- )
  SWAP CELLS OVER + 1000 SWAP !
  SWAP CELLS+ !
PROC;
: (textfromitem) ( z i ctl mask off off2 msg buf item -- )
  0 (gettext) itemsend ;

PROC: (settext) { z off1 off2 item -- }
  z item off1 CELLS!
  z ZLEN item off2 CELLS!
PROC;
: (texttoitem) ( z i ctl mask off off2 msg buf item -- )
  0 (settext) itemsend ;

PROC: (setstatemask) ( off1 off2 item -- )
  SWAP CELLS + -1 SWAP ! DROP
PROC;
: (statefromitem) ( i ctl mask off off2 msg buf item -- n)
  (getn) (setstatemask) itemsend ;

PROC: (setstate) ( n off1 off2 item -- )
  >R CELLS R@ + -1 SWAP !
  CELLS R> + !
PROC;
: (statetoitem) ( n ctl mask off off2 msg buf item -- )
  0 (setstate) itemsend ;

: fromitem ( i ctl mask off msg -- n ) 0 SWAP alloc-item DUP (fromitem) ;
: toitem ( n i ctl mask off msg -- ) 0 SWAP alloc-item DUP (toitem) ;
: textfromitem ( z i ctl mask off1 off2 msg -- ) alloc-item DUP (textfromitem) ;
: texttoitem ( z i ctl mask off1 off2 msg -- ) alloc-item DUP (texttoitem) ;

\ ------------------------------------
\ ���������

-1 == callback
-2 == none

\ : adjust-header { ctl \ [ 4 7 + CELLS ] hdlayout -- }
\  ctl win-size hdlayout 3 CELLS! hdlayout 2 CELLS!
\  hdlayout 60 DUMP
\  0 hdlayout W: hdm_layout ctl send . ;

: header-add { flags text bitmap param i ctl \ [ 7 CELLS ] hditem -- }
  hditem init->>
  \ mask
  (* hdi_format hdi_lparam hdi_height *)
  bitmap none <> IF W: hdi_bitmap ELSE W: hdi_text THEN OR >>
  \ height
  bitmap none <> IF bitmap bitmap-size ELSE text ctl text-size THEN PRESS  >>		
  \ text, bitmap, textmax
  bitmap none <> IF 0 >> bitmap >> 0 >> ELSE text >> 0 >> text ZLEN >> THEN
  \ format
  flags bitmap none <> IF W: hdf_bitmap ELSE W: hdf_string THEN OR >>
  param >>
  i hditem W: hdm_insertitema ctl send DROP ;

: header-del ( i ctl -- ) >R 0 W: hdm_deleteitem R> send DROP ;

: header-num ( ctl -- )  W: hdm_getitemcount ?send ;

: header-get ( ... -- n )  W: hdm_getitema fromitem ;
: header-set ( ... -- n )  W: hdm_setitema toitem ;

: get-hdwidth ( i ctl -- n ) W: hdi_width 1 header-get ;
: set-hdwidth ( n i ctl -- ) W: hdi_width 1 header-set ;
: get-hdheight ( i ctl -- n ) W: hdi_height 1 header-get ;
: set-hdheight ( n i ctl -- ) W: hdi_height 1 header-set ;
: get-hdparam ( i ctl -- n ) W: hdi_lparam 6 header-get ;
: set-hdparam ( n i ctl -- ) W: hdi_lparam 6 header-set ;
: get-hdflags ( i ctl -- n ) W: hdi_format 5 header-get ;
: set-hdflags ( n i ctl -- ) W: hdi_format 5 header-set ;

: set-hdalign ( align i ctl -- )
  ROT >R 2DUP get-hdflags W: hdf_justifymask INVERT AND
  R> CASE
  center OF W: hdf_center ENDOF
  right OF W: hdf_right ENDOF
  DROP W: hdf_left
  END-CASE
  OR -ROT set-hdflags ;
: get-hdalign ( i ctl -- align )
  get-hdflags W: hdf_justifymask AND CASE
  W: hdf_right OF right ENDOF
  W: hdf_center OF center OF
  DROP left
  END-CASE ;

: get-hdimage ( i ctl -- bmp/0) W: hdi_bitmap 3 header-get ;
: set-hdimage { bmp i ctl \ [ 7 CELLS ] item -- }
  (* hdi_bitmap hdi_format *) item !
  bitmap item 3 CELLS!
  i ctl get-hdflags W: hdf_string INVERT AND W: hdf_bitmap OR item 5 CELLS!
  i item W: hdm_setitema ctl send DROP ;

: get-hdtext ( a i ctl -- )
  W: hdi_text 2 4 W: hdm_getitema textfromitem ;
: set-hdtext { z i ctl \ [ 7 CELLS ] item -- }
  (* hdi_text hdi_format *) item ! 
  z item 2 CELLS!
  z ZLEN item 4 CELLS!
  i ctl get-hdflags W: hdf_bitmap INVERT AND W: hdf_string OR item 5 CELLS!
  i item W: hdm_setitema ctl send DROP ;

: header ( style -- ctl )
  container " SysHeader32" ROT create-control >R
  W: hdn_itemclicka R@ -defcommand!
  ['] header-add R@ -addproc!
  ['] header-del R@ -delproc!
  ['] header-num R@ -numproc!
  ['] get-hdwidth  ['] set-hdwidth  -iwidth  R@ setitem
  ['] get-hdparam  ['] set-hdparam  -iparam  R@ setitem
  ['] get-hdheight ['] set-hdheight -iheight R@ setitem
  ['] get-hdflags  ['] set-hdflags  -iflags  R@ setitem
  ['] get-hdalign  ['] set-hdalign getset-flag -align R@ setflagitem
  ['] get-hdimage  ['] set-hdimage -iimage R@ setitem
  ['] get-hdtext   ['] set-hdtext   -itext   R@ setitem
  R> ;

\ --------------------------------
\ ��������

container table tabctl
  item -maxwidth   \ ������ ����� ������� �����
  item -maxheight  \ ������ ����� ������� �����
endtable

PROC: calc-tab-size { tab \ [ 4 CELLS ] rect -- w h }
  rect 0!
  0 rect 1 CELLS!
  tab -maxwidth@  rect 2 CELLS!
  tab -maxheight@ rect 3 CELLS!
  TRUE rect W: tcm_adjustrect tab send DROP
  rect 2 CELLS@ rect @ - rect 3 CELLS@ rect 1 CELLS@ -
PROC;

: tc-add { grid text image i ctl \ [ 7 CELLS ] item -- }
  W: tcif_param item !
  grid item 6 CELLS!
  text none <> IF 
     W: tcif_text item OR!
     text item 3 CELLS!
     text ZLEN item 4 CELLS!
  THEN
  image none <> IF
    W: tcif_image item OR!
    image item 5 CELLS!
  THEN
  i item W: tcm_insertitema ctl send DROP 
  grid grid-size
  ctl -maxheight@ MAX ctl -maxheight!
  ctl -maxwidth@ MAX ctl -maxwidth!
  ctl calc-tab-size EXECUTE ctl resize
;

: tc-del ( i ctl -- ) >R 0 W: tcm_deleteitem R> send DROP ;
: tc-num ( ctl -- )  W: tcm_getitemcount ?send ;

: tab-get ( ... i ctl -- n ) W: tcm_getitema fromitem ;
: tab-set ( ... i ctl -- ) W: tcm_setitema toitem ;

: get-tctext ( z i ctl -- ) W: tcif_text 3 4 W: tcm_getitema textfromitem ;
: set-tctext ( z i ctl -- ) W: tcif_text 3 4 W: tcm_setitema texttoitem ;

: get-tcimage ( i ctl -- n ) W: tcif_image 5 tab-get ;
: get-tcparam ( i ctl -- n ) W: tcif_param 6 tab-get ;
: set-tcimage ( n i ctl -- ) W: tcif_image 5 tab-set ;
: set-tcparam ( n i ctl -- ) W: tcif_param 6 tab-set ;

: get-tcsel ( ctl -- n ) W: tcm_getcursel ?send ;
: set-tcsel ( n ctl -- ) W: tcm_setcursel wsend DROP ;

: get-tcil ( 0 ctl -- n ) PRESS W: tcm_getimagelist ?send ;
: set-tcil ( n 0 ctl -- ) PRESS W: tcm_setimagelist lsend DROP ;

: map-current-tab-grid { tab \ [ 4 CELLS ] rect -- }
  \ ������ ������������ ��������
  tab child-win-rect rect 3 CELLS! rect 2 CELLS! rect 1 CELLS! rect !
  \ ����������� ���������� ����������� ����� ��� �����
  FALSE rect W: tcm_adjustrect tab send DROP
  \ ������������ ���������
  rect @ rect 1 CELLS@
  tab -parent@ ?DUP IF -minustop@ ELSE 0 THEN DUP >R - 2DUP
  rect 3 CELLS@ SWAP -  SWAP rect 2 CELLS@ SWAP -  SWAP R> -
  tab -selected@ tab -iparam@ map-grid
;

: hide-selected-tab ( ctl -- )
  DUP -selected@ SWAP -iparam@ hide-grid ;

: show-selected-tab ( ctl -- )
  DUP map-current-tab-grid
  DUP -selected@ SWAP -iparam@ show-grid ;

PROC: tab-show { ctl -- }
  ctl -selected@ -1 = IF EXIT THEN
  ctl show-selected-tab
  ctl winshow
PROC;

PROC: tab-hide { ctl -- }
  ctl -selected@ -1 = IF EXIT THEN
  ctl hide-selected-tab
  ctl winhide
PROC;

PROC: tab-resize ( x y ctl -- )
  DUP >R resize R> map-current-tab-grid
PROC;

: switch-tab { i ctl -- }
   ctl hide-selected-tab
   i 0 W: TCM_SETCURSEL ctl send DROP
   ctl show-selected-tab ;



MESSAGES: tabcontrol-notify

M: tcn_selchanging
   thisctl hide-selected-tab
M;

M: tcn_selchange
   thisctl -command@ EXECUTE
   thisctl show-selected-tab
M;

MESSAGES;

: tabcontrol ( style -- ctl )
  tabctl " SysTabControl32" ROT create-control >R
  tabcontrol-notify R@ -notify!
  ['] tc-add R@ -addproc!
  ['] tc-del R@ -delproc!
  ['] tc-num R@ -numproc!
  ['] get-tctext  ['] set-tctext  -itext  R@ setitem
  ['] get-tcimage ['] set-tcimage -iimage R@ setitem
  ['] get-tcparam ['] set-tcparam -iparam R@ setitem
  ['] get-tcsel   ['] set-tcsel   -selected R@ setitem
  ['] get-tcil    ['] set-tcil    -imagelist R@ setitem
  calc-tab-size R@ -calcsize!
  tab-show R@ -ctlshow!
  tab-hide R@ -ctlhide!
  tab-resize R@ -ctlresize!
  0 R@ -maxwidth!
  0 R@ -maxheight!
  R> ;

\ -----------------------------------------
\ ������ ������������

container table toolbarctl
  item -istate      	 getset \ ����� ��������
endtable

\ :NONAME

: tb-getil ( what ctl -- il )
  SWAP CASE
  1 OF W: tb_gethotimagelist ENDOF
  2 OF W: tb_getdisabledimagelist ENDOF
  DROP W: tb_getimagelist
  END-CASE ?send ;
: tb-setil ( il what ctl -- i )
  SWAP CASE
  1 OF W: tb_sethotimagelist ENDOF
  2 OF W: tb_setdisabledimagelist ENDOF
  DROP W: tb_setimagelist
  END-CASE lsend ;

: alloc-tbitem ( -- buf item) alloc-item DUP 8 CELLS SWAP ! DUP CELL+ ;

: tb-get ( ... i ctl -- n ) 0 W: tb_getbuttoninfoa alloc-tbitem (fromitem) ;
: tb-set ( ... i ctl -- ) 0 W: tb_setbuttoninfoa alloc-tbitem (toitem) ;

: tb-getimage ( i ctl -- n) W: tbif_image 2 tb-get ;
: tb-setimage ( n i ctl -- ) W: tbif_image 2 tb-set ;
: tb-gettext ( z i ctl -- )
  W: tbif_text 5 6 W: tb_getbuttoninfoa alloc-tbitem (textfromitem) ;
: tb-settext ( z i ctl -- )
  W: tbif_text 5 6 W: tb_setbuttoninfoa alloc-tbitem (texttoitem) ;
: tb-getstate ( i ctl -- c)
  W: tbif_state 12 0 W: tb_getbuttoninfoa alloc-tbitem (bytefromitem) ;
: tb-setstate ( c i ctl -- )
  W: tbif_state 12 0 W: tb_setbuttoninfoa alloc-tbitem (bytetoitem) ;
: tb-getflags ( i ctl -- c)
  W: tbif_style 13 0 W: tb_getbuttoninfoa alloc-tbitem (bytefromitem) ;
: tb-setflags ( c i ctl -- )
  W: tbif_style 13 0 W: tb_setbuttoninfoa alloc-tbitem (bytetoitem) ;

: tb-num ( ctl -- n ) W: tb_buttoncount ?send ;
: tb-del ( i ctl -- ) W: tb_deletebutton wsend DROP ;

: tb-add { text image style proc n ctl \ [ 5 CELLS ] tbbutton -- }
  tbbutton init->>
  image >>				\ ��������
  n >>					\ ��� ������
  W: tbstate_enabled C>>        	\ ��������� ������
  style C>>				\ ����� ������
  0 W>>
  proc >>				\ lparam
  text none <> IF text ELSE 0 THEN >>	\ ������
  1 tbbutton W: tb_addbuttons ctl send DROP ;

: separate { ctl \ [ 4 CELLS ] tbbutton -- }
  1 DATA[ 0 , 0 , 0 C, W: tbstyle_sep C, 0 , ]DATA W: tb_addbuttons ctl send DROP ;

: toolbar-sizes ( buttx butty imagex imagey ctl -- )
  >R
  16 LSHIFT OR R@ W: tb_setbitmapsize lsend DROP
  16 LSHIFT OR R> W: tb_setbuttonsize lsend DROP ;

: add-std-bitmap { id ctl \ [ 2 CELLS ] buf -- n }
  W: hinst_commctrl buf !  id buf CELL+ !
  1 buf W: tb_addbitmap ctl send ;

: add-res-bitmaps { n id ctl \ [ 2 CELLS ] buf -- n }
  IMAGE-BASE buf ! id buf CELL+ !
  n buf W: tb_addbitmap ctl send ;

\ ������ �� �������� �������� wm_notify, ������������ ������ - wm_command
: dropdown? ( -- ?) message W: wm_notify = ;

: tb-command
  dropdown? IF lparam @ ELSE wparam LOWORD THEN
  thisctl W: tbif_lparam 4 tb-get EXECUTE ;

: toolbar ( style -- ctl )
  toolbarctl " ToolbarWindow32" ROT create-control >R
  20 R@ W: tb_buttonstructsize wsend DROP
  (* tbstyle_ex_drawddarrows tbstyle_ex_mixedbuttons tbstyle_ex_hideclippedbuttons *)
  R@ W: tb_setextendedstyle lsend DROP
  ['] tb-add R@ -addproc!
  ['] tb-del R@ -delproc!
  ['] tb-num R@ -numproc!
  ['] tb-getimage ['] tb-setimage -iimage R@ setitem
  ['] tb-gettext  ['] tb-settext  -itext  R@ setitem
  ['] tb-getflags ['] tb-setflags -iflags R@ setitem
  ['] tb-getstate ['] tb-setstate -istate R@ setitem
  ['] tb-getil    ['] tb-setil    -imagelist R@ setitem
  ['] tb-command R@ -command!
  W: bn_clicked R@ -defcommand!
  R> ;

: add-toolbar ( ctl win -- )
  >R DUP win-size R@ -minustop! DROP 
  DUP R> -toolbar! winshow ;

: create-toolbar ( style win -- ) >R toolbar R> add-toolbar ;

\ ----------------------------------
\ �������

toolbarctl table treeviewctl
  item -iselimage	getset	\ �������� ��� ���������� ��������
endtable

: tv-add
  { text image selimage param after parent ctl \ [ 12 CELLS ] tvins tvitem -- i }
  tvins 2 CELLS+ TO tvitem
  parent tvins !
  after tvins 2 CELLS!
  (* tvif_param *) tvitem !
  param tvitem 9 CELLS!
  text none <> IF 
    W: tvif_text tvitem OR!
    text tvitem 4 CELLS!
    text ZLEN tvitem 5 CELLS!
  THEN
  image none <> IF 
    W: tvif_image tvitem OR!
    image tvitem 6 CELLS!
  THEN
  selimage none <> IF 
    W: tvif_selectedimage tvitem OR!
    selimage tvitem 7 CELLS!
  THEN
  0 tvins W: tvm_insertitema ctl send ;

: tv-del ( i ctl -- ) W: tvm_deleteitem lsend DROP ;
: tv-num ( ctl -- n ) W: tvm_getcount ?send ;

: alloc-tvitem ( i ctl mask offset -- i ctl mask offset buf )
  SWAP W: tvif_handle OR SWAP alloc-item
  4 PICK OVER 1 CELLS! ;

: tv-get ( i ctl mask offset -- n )
  alloc-tvitem >R 0 W: tvm_getitema R> DUP (fromitem) ;
: tv-set ( n i ctl mask offset -- ) 
  alloc-tvitem >R 0 W: tvm_setitema R> DUP (toitem) ;

: tv-getparam ( i ctl -- n ) W: tvif_param 9 tv-get ;
: tv-setparam ( n i ctl -- n ) W: tvif_param 9 tv-set ;
: tv-getimage ( i ctl -- n ) W: tvif_image 6 tv-get ;
: tv-setimage ( n i ctl -- n ) W: tvif_image 6 tv-set ;
: tv-getselimage ( i ctl -- n ) W: tvif_selectedimage 7 tv-get ;
: tv-setselimage ( n i ctl -- n ) W: tvif_selectedimage 7 tv-set ;
: tv-getitext ( z i ctl -- ) W: tvif_text 4
  alloc-tvitem >R 5 W: tvm_getitema R> DUP (textfromitem) ;
: tv-setitext ( z i ctl -- ) W: tvif_text 4
  alloc-tvitem >R 5 W: tvm_setitema R> DUP (texttoitem) ;

: tv-getstate ( i ctl -- n ) W: tvif_state 2
  alloc-tvitem >R 3 W: tvm_getitema R> DUP (statefromitem) ;
: tv-setstate ( n i ctl -- ) W: tvif_state 2
  alloc-tvitem >R 3 W: tvm_setitema R> DUP (statetoitem) ;
  
: tv-getselect ( ctl -- i ) 
  >R W: tvgn_caret W: tvi_root W: tvm_getnextitem R> send ;
: tv-setselect ( i ctl -- ) 
  >R W: tvgn_caret SWAP W: tvm_selectitem R> send DROP ;

: tv-getil ( what ctl -- il ) W: tvm_getimagelist wsend ;
: tv-setil ( il what ctl -- ) >R SWAP W: tvm_setimagelist R> send DROP ;

: treeview ( style -- ctl )
  treeviewctl " SysTreeView32" ROT W: ws_ex_clientedge create-control-exstyle >R
  white R@ -bgcolor!
  white R@ W: tvm_setbkcolor lsend DROP
  black R@ -color!
  W: tvn_selchangeda R@ -defcommand!
  ['] tv-getselect ['] tv-setselect -selected R@ setitem
  ['] tv-add R@ -addproc!
  ['] tv-del R@ -delproc!
  ['] tv-num R@ -numproc!
  ['] tv-getparam ['] tv-setparam -iparam R@ setitem
  ['] tv-getimage ['] tv-setimage -iimage R@ setitem
  ['] tv-getselimage ['] tv-setselimage -iselimage R@ setitem
  ['] tv-getitext ['] tv-setitext -itext R@ setitem
  ['] tv-getstate ['] tv-setstate -istate R@ setitem
  ['] tv-getil    ['] tv-setil    -imagelist R@ setitem
  R> ;

: walk-tree { proc from ctl -- } 
  W: tvgn_child from W: tvm_getnextitem ctl send
  BEGIN
    ?DUP
  WHILE
    TO from
    from ctl proc EXECUTE
    W: tvgn_next from W: tvm_getnextitem ctl send
  REPEAT ;

\ -------------------------------------------
\ ������ ������

toolbarctl table listviewctl
  item -exstyle  getset \ ����������� �����
  item -isubitem getset	\ ����������
  item -ctext	getset	\ ����� �������
  item -cflags	getset	\ ����� �������
  item -cwidth	getset	\ ������ �������
  item -csubitem getset	\ ����������, ����������� � �������
  item -cimage	getset	\ �������� �������
  item -corder	getset	\ ������� �������
endtable

: lv-getcol W: lvm_getcolumna fromitem ;
: lv-setcol W: lvm_setcolumna toitem ;

:NONAME \ getexstyle ( ctl -- n)
 W: lvm_getextendedlistviewstyle ?send ;
:NONAME \ setexstyle ( n ctl -- )
 W: lvm_setextendedlistviewstyle lsend DROP ;
-exstyle listviewctl setitem

:NONAME \ get-colflags ( i ctl -- n)
  W: lvcf_fmt 1 lv-getcol ;
:NONAME \ set-colflags ( n i ctl -- n)
  W: lvcf_fmt 1 lv-setcol ;
-cflags listviewctl setitem

:NONAME \ get-colwidth ( i ctl -- n)
  W: lvcf_width 2 lv-getcol ;
:NONAME \ set-colwidth ( n i ctl -- n)
  W: lvcf_width 2 lv-setcol ;
-cwidth listviewctl setitem

:NONAME \ get-colsubitem ( i ctl -- n)
  W: lvcf_subitem 5 lv-getcol ;
:NONAME \ set-colsubitem ( n i ctl -- n)
  W: lvcf_subitem 5 lv-setcol ;
-csubitem listviewctl setitem

:NONAME \ get-colimage ( i ctl -- n)
  W: lvcf_image 6 lv-getcol ;
:NONAME \ set-colimage ( n i ctl -- n)
  W: lvcf_image 6 lv-setcol ;
-csubitem listviewctl setitem

:NONAME \ get-colorder ( i ctl -- n)
  W: lvcf_order 7 lv-getcol ;
:NONAME \ set-colorder ( n i ctl -- n)
  W: lvcf_order 7 lv-setcol ;
-csubitem listviewctl setitem

:NONAME \ get-coltext ( z i ctl -- n)
  W: lvcf_text 3 4 W: lvm_getcolumna textfromitem ;
:NONAME \ set-coltext ( z i ctl -- n)
  W: lvcf_text 3 4 W: lvm_setcolumna texttoitem  ;
-ctext listviewctl setitem

: alloc-lvitem ( i ctl mask offset -- i ctl mask offset buf )
  alloc-item
  4 PICK OVER 1 CELLS! ;

:NONAME \ get-isubitem ( z i si ctl -- )
  SWAP >R W: lvif_text 5 alloc-lvitem >R 6 W: lvm_getitema
  R> R> OVER 2 CELLS! DUP (textfromitem)
;
:NONAME \ set-isubitem ( z i si ctl -- )
  SWAP >R W: lvif_text 5 alloc-lvitem >R 6 W: lvm_setitema
  R> R> OVER 2 CELLS! DUP (texttoitem)
; -isubitem listviewctl setitem

: lv-del ( i ctl -- ) W: lvm_deleteitem wsend DROP ;
: lv-num ( ctl -- ) W: lvm_getitemcount ?send ;
: lv-add { text image param ctl \ [ 9 CELLS ] lvitem -- }
  lvitem init->>
  W: lvif_param
  text  none <> IF W: lvif_text  OR THEN
  image none <> IF W: lvif_image OR THEN
  >>
  0 >>		\ i
  0 >>		\ subitem
  0 >> 0 >>	\ state, statemask
  text none = IF
    0 >>
    0 >>
  ELSE
    text callback = IF
      W: lpstr_textcallbacka >>
      0 >>
    ELSE
      text >>       \ text
      text ZLEN >>	\ text#
    THEN
  THEN
  image none = IF
    0 >>
  ELSE
    image callback = IF W: i_imagecallback ELSE image THEN >>	\ image#
  THEN
  param >>	\ param
  0 lvitem W: lvm_insertitema ctl send DROP ;

: lv-getil ( what ctl -- il ) W: lvm_getimagelist wsend ;
: lv-setil ( il what ctl -- ) >R SWAP W: lvm_setimagelist R> send DROP ;

: lv-getcolor ( ctl -- ) W: lvm_gettextcolor ?send >bgr ;
: lv-getbgcolor ( ctl -- ) W: lvm_getbkcolor ?send >bgr ;
: lv-setcolor ( color ctl -- ) SWAP >bgr SWAP W: lvm_settextcolor lsend DROP ; 
: lv-setbgcolor ( color ctl -- ) SWAP >bgr SWAP W: lvm_setbkcolor lsend DROP ;

: lv-get ( i ctl mask offset -- )
  alloc-lvitem >R 0 W: lvm_getitema R> DUP (fromitem) ;
: lv-set ( n i si ctl mask offset -- )
  alloc-lvitem >R 0 W: lvm_setitema R> DUP (toitem) ;

: lv-gettext ( z i ctl -- ) W: lvif_text 5
  alloc-lvitem >R 6 W: lvm_getitema R> DUP (textfromitem) ;
: lv-settext ( z i ctl -- ) W: lvif_text 5
  alloc-lvitem >R 6 W: lvm_setitema R> DUP (texttoitem) ;

: lv-getstate ( i ctl -- n ) W: lvif_state 3
  alloc-lvitem >R 4 W: lvm_getitema R> DUP (statefromitem) ;
: lv-setstate ( n i ctl -- ) W: lvif_state 3
  alloc-lvitem >R 4 W: lvm_setitema R> DUP (statetoitem) ;

: lv-getparam ( i ctl -- n ) W: lvif_param 8 lv-get ;
: lv-setparam ( n i ctl -- ) W: lvif_param 8 lv-set ;
: lv-getimage ( i ctl -- n ) W: lvif_image 7 lv-get ;
: lv-setimage ( n i ctl -- ) W: lvif_image 7 lv-set ;

\ -1 �� �������� ������
\ < -1 ����� ���������� (�� ������)
\ > -1 ������ ����������� ��������
: lv-getselect ( ctl -- n/i/-1 )
  DUP >R W: lvm_getselectedcount ?send
  ?DUP IF
    DUP 1 = IF
      DROP -1  W: lvni_selected W: lvm_getnextitem R@ send
    ELSE
      NEGATE
    THEN
  ELSE
    -1
  THEN R> DROP ;
: lv-setselect ( i ctl -- ) 2DUP lv-getstate W: lvis_selected OR -ROT lv-setstate ;

: listview ( style -- ctl )
  listviewctl " SysListView32" ROT W: ws_ex_clientedge create-control-exstyle >R
  ['] lv-add R@ -addproc!
  ['] lv-del R@ -delproc!
  ['] lv-num R@ -numproc!
  ['] lv-getil   ['] lv-setil   -imagelist R@ setitem
  R@ -bgbrush@ DeleteObject DROP
  ['] lv-getcolor   ['] lv-setcolor   getset-flag -color   R@ setflagitem
  ['] lv-getbgcolor ['] lv-setbgcolor getset-flag -bgcolor R@ setflagitem
  ['] lv-getparam   ['] lv-setparam   -iparam  R@ setitem
  ['] lv-getimage   ['] lv-setimage   -iimage  R@ setitem
  ['] lv-gettext    ['] lv-settext    -itext   R@ setitem
  ['] lv-getstate   ['] lv-setstate   -istate  R@ setitem
  ['] lv-getselect  ['] lv-setselect  -selected R@ setitem
  W: nm_click R@ -defcommand!
  R> ;

: prepare-listview ( n ctl -- ) W: lvm_setitemcount wsend DROP ;
: clear-listview ( ctl -- ) W: lvm_deleteallitems ?send DROP ;

: lv-style ( ctl style -- )
  OVER -style@ W: lvs_typemask INVERT AND OR SWAP -style! ;

: icon-view ( ctl -- ) W: lvs_icon lv-style ;
: smallicon-view ( ctl -- ) W: lvs_smallicon lv-style ;
: list-view ( ctl -- ) W: lvs_list lv-style ;
: report-view ( ctl -- ) W: lvs_report lv-style ;

: arrange-listview ( type ctl -- ) W: lvm_arrange wsend DROP ;

: lv-param>i { param ctl \  [ 6 CELLS ] lvfind -- i }
  W: lvfi_param lvfind !
  param lvfind 2 CELLS!
  -1 lvfind W: lvm_finditema ctl send ;

: add-column { text subitem i ctl \ [ 6 CELLS ] lvcol -- }
  lvcol init->>
  (* lvcf_fmt lvcf_subitem lvcf_text lvcf_width *) >>	\ �����
  W: lvcfmt_left >>					\ ������������
  0 text W: lvm_getstringwidtha ctl send 20 + >>	\ ������ �������							\ ������ �������
  text >>						\ �����
  text ZLEN >>						\ �-�� ��������
  subitem >>						\ �����������
  i lvcol W: lvm_insertcolumna ctl send DROP ;

: del-column ( i ctl -- ) W: lvm_deletecolumn wsend DROP ;

: walk-selected { proc ctl -- }
  -1
  BEGIN
    W: lvni_selected W: lvm_getnextitem ctl send DUP -1 <>
  WHILE
    DUP proc EXECUTE
  REPEAT DROP ;

\ -------------------------------
\ ����������� ���������
: create-tooltip ( style -- )
  >R  W: icc_tab_classes initcc
  control " tooltips_class32" R> 0 create-control-exstyle-notchild
  -sysfont
  TO common-tooltip ;

:NONAME { z ctl mess \ [ 10 CELLS ] ti -- }
  ti init->>
  10 CELLS >>
  (* ttf_idishwnd ttf_subclass *) >>
  0 >>
  ctl -hwnd@ >>
  0 >> 0 >> 0 >> 0 >>
  0 >>
  z >>
  ti common-tooltip mess lsend DROP ; 
TO common-tooltip-op

\ ��� �������� � �������������
\ initial - ������� ����� ����� "���������" ���� ������ ������ � ������� �����
\ autopop - ������� ������� ������, ���� ������ ������ ��������
\ reshow - ������� ����� ����� "���������" ���� ������ �������� �� ���������
\ �� ��������� t, 10*t, t/5, ��� t - ����� ���������
\ ����� �������� �������� � ��������� - ���������� -1
: set-tooltip-delay { initial autopop reshow tooltip -- }
  W: TTDT_INITIAL initial W: TTM_SETDELAYTIME tooltip send DROP 
  W: TTDT_AUTOPOP autopop W: TTM_SETDELAYTIME tooltip send DROP
  W: TTDT_RESHOW  reshow  W: TTM_SETDELAYTIME tooltip send DROP 
  ;

\ -------------------------------
\EOF

\ -------------------------------
\ ������

: rb-num ( ctl -- ) W: rb_getbandcount ?send ;
: rb-del ;
: rb-add { text image style bmp child where ctl \ [ 14 CELLS ] buf }
  buf init->>
  14 CELLS >>				\ size of
  (* rbbim_style rbbim_child *) >>	\ mask
  style >>				\ style
  0 >>	0 >>				\ colors
  text none <> IF
    W: rbbim_text buf CELL+ OR!
    text >>  text ZLEN >>		\ text, text#
  THEN
  image none <> IF
    W: rbbim_image buf CELL+ OR!
    image >>				\ image
  THEN
  child -hwnd@ >>			\ child hwnd
  0 >> 0 >> 0 >>
  bmp none <> IF
    W: rbbim_background buf CELL+ OR!
    bmp >>				\ bitmap
  THEN
  where buf W: rb_insertbanda ctl send DROP ;

: rb-getil { zero ctl \ [ 3 CELLS ] barinfo -- }
  barinfo init->>
  3 CELLS >>
  W: rbim_imagelist >>
  barinfo ctl W: rb_getbarinfo lsend DROP
  barinfo 2 CELLS@ ;

: rb-setil { il zero ctl \ [ 3 CELLS ] barinfo -- }
  barinfo init->>
  3 CELLS >>
  W: rbim_imagelist >>
  il >>
  barinfo ctl W: rb_setbarinfo lsend DROP ;

: rebar ( style -- ctl) 
  W: icc_cool_classes initcc
  container " ReBarWindow32" ROT create-control >R
  ['] rb-add R@ -addproc!
  ['] rb-del R@ -delproc!
  ['] rb-num R@ -numproc!
  ['] rb-getil  ['] rb-setil -imagelist R@ setitem
  R> ;
