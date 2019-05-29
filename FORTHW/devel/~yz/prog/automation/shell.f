REQUIRE create-object ~ yz / lib / automation.f

0 VALUE shell
0 VALUE folder
0 VALUE item

: Zr (zn -)
  OVER .ansiz SWAP ZLEN - 0 MAX SPACES;

: show-desktop

 Com-init DROP

 "Shell.Application" create-object 
 IF "Cannot start Shell.Application object" .ansiz BYE THEN
 TO shell

 arg (17 _int) arg shell :: NameSpace> 
 DROP TO folder

 arg () folder :: Items>
 Drop

 "Object" 20 ZR "Name" 25 ZR "Size" 12 ZR "Free" 12 ZR CR
 69 0 DO c: - EMIT LOOP CR
 FOREACH
   OBJ-I DROP TO item
   arg (item _obj 1 _int) arg folder :: GetDetailsOf>
   DROP DUP 20 ZR FREEMEM 
   arg (item _obj 0 _int) arg folder :: GetDetailsOf>
   DROP DUP 25 ZR FREEMEM 
   arg (item _obj 2 _int) arg folder :: GetDetailsOf>
   DROP DUP 12 ZR FREEMEM 
   arg (item _obj 3 _int) arg folder :: GetDetailsOf>
   DROP DUP 12 ZR FREEMEM 
   CR
   item release
 NEXT

KEY DROP

folder release
shell release

COM-destroy;

show-desktop

BYE