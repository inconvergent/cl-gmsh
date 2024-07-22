## `gmsh/cam:@pos`
```
 ; GMSH/CAM:@POS
 ;   [symbol]
 ; 
 ; @POS names a compiled function:
 ;   Lambda-list: (C &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get pos. as 3 values.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF @POS) has a complex setf-expansion:
 ;   Lambda-list: (C1 &OPTIONAL (I2 0))
 ;   Documentation:
 ;     set 3 values in struct field CAM-POS.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@u`
```
 ; GMSH/CAM:@U
 ;   [symbol]
 ; 
 ; @U names a compiled function:
 ;   Lambda-list: (C &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get u. as 3 values.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF @U) has a complex setf-expansion:
 ;   Lambda-list: (C19 &OPTIONAL (I20 0))
 ;   Documentation:
 ;     set 3 values in struct field CAM-U.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@up`
```
 ; GMSH/CAM:@UP
 ;   [symbol]
 ; 
 ; @UP names a compiled function:
 ;   Lambda-list: (C &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get up. as 3 values.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF @UP) has a complex setf-expansion:
 ;   Lambda-list: (C7 &OPTIONAL (I8 0))
 ;   Documentation:
 ;     set 3 values in struct field CAM-UP.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@v`
```
 ; GMSH/CAM:@V
 ;   [symbol]
 ; 
 ; @V names a compiled function:
 ;   Lambda-list: (C &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get v. as 3 values.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF @V) has a complex setf-expansion:
 ;   Lambda-list: (C25 &OPTIONAL (I26 0))
 ;   Documentation:
 ;     set 3 values in struct field CAM-V.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@vpn`
```
 ; GMSH/CAM:@VPN
 ;   [symbol]
 ; 
 ; @VPN names a compiled function:
 ;   Lambda-list: (C &OPTIONAL (I 0))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get vpn. as 3 values.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF @VPN) has a complex setf-expansion:
 ;   Lambda-list: (C13 &OPTIONAL (I14 0))
 ;   Documentation:
 ;     set 3 values in struct field CAM-VPN.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:cam`
```
:missing:

 ; GMSH/CAM:CAM
 ;   [symbol]
 ; 
 ; CAM names the structure-class #<STRUCTURE-CLASS GMSH/CAM:CAM>:
 ;   Class precedence-list: GMSH/CAM:CAM, STRUCTURE-OBJECT,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Slots:
 ;     GMSH/CAM::NAV-MODE
 ;       Type: KEYWORD
 ;       Initform: :PAN
 ;     GMSH/CAM::PROJ-MODE
 ;       Type: KEYWORD
 ;       Initform: :PERSP
 ;     GMSH/CAM::POS
 ;       Type: VEQ:3FVEC
 ;       Initform: (VEQ:F3$PT 150.0 150.0 50.0)
 ;     GMSH/CAM::UP
 ;       Type: VEQ:3FVEC
 ;       Initform: (VEQ:F3$PT 0.0 1.0 0.0)
 ;     GMSH/CAM::VPN
 ;       Type: VEQ:3FVEC
 ;       Initform: (VEQ:F3$PT 0.0 0.0 1.0)
 ;     GMSH/CAM::PAR
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F$~ (6)
 ;                   100.0
 ;                   10.0
 ;                   1000.0
 ;                   10.0
 ;                   3.0
 ;                   100.0)
 ;     GMSH/CAM::U
 ;       Type: VEQ:3FVEC
 ;       Initform: (VEQ:F3$VAL 0.0)
 ;     GMSH/CAM::V
 ;       Type: VEQ:3FVEC
 ;       Initform: (VEQ:F3$VAL 0.0)
```

## `gmsh/cam:export-data`
```
 ; GMSH/CAM:EXPORT-DATA
 ;   [symbol]
 ; 
 ; EXPORT-DATA names a compiled function:
 ;   Lambda-list: (C)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM) (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     serialize cam. see import-data.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:import-data`
```
 ; GMSH/CAM:IMPORT-DATA
 ;   [symbol]
 ; 
 ; IMPORT-DATA names a compiled function:
 ;   Lambda-list: (O)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     load serialized cam. see export-data.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:make`
```
:missing:

 ; GMSH/CAM:MAKE
 ;   [symbol]
 ; 
 ; MAKE names a compiled function:
 ;   Lambda-list: (&KEY ((NAV-MODE NAV-MODE) PAN)
 ;                 ((PROJ-MODE PROJ-MODE) PERSP)
 ;                 ((POS POS) (F3$PT 150.0 150.0 50.0))
 ;                 ((UP UP) (F3$PT 0.0 1.0 0.0))
 ;                 ((VPN VPN) (F3$PT 0.0 0.0 1.0))
 ;                 ((PAR PAR)
 ;                  (F$~ (6)
 ;                    100.0
 ;                    10.0
 ;                    1000.0
 ;                    10.0
 ;                    3.0
 ;                    100.0))
 ;                 ((U U) (F3$VAL 0.0)) ((V V) (F3$VAL 0.0)))
 ;   Derived type: (FUNCTION
 ;                  (&KEY (:NAV-MODE KEYWORD) (:PROJ-MODE KEYWORD)
 ;                   (:POS (SIMPLE-ARRAY SINGLE-FLOAT #1=(3)))
 ;                   (:UP (SIMPLE-ARRAY SINGLE-FLOAT #1#))
 ;                   (:VPN (SIMPLE-ARRAY SINGLE-FLOAT #1#))
 ;                   (:PAR (SIMPLE-ARRAY SINGLE-FLOAT))
 ;                   (:U (SIMPLE-ARRAY SINGLE-FLOAT #1#))
 ;                   (:V (SIMPLE-ARRAY SINGLE-FLOAT #1#)))
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:nav/around!`
```
 ; GMSH/CAM:NAV/AROUND!
 ;   [symbol]
 ; 
 ; NAV/AROUND! names a compiled function:
 ;   Lambda-list: (C AX VAL)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM KEYWORD SINGLE-FLOAT) *)
 ;   Documentation:
 ;     rotate around cam axis.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:nav/axis!`
```
:missing:

 ; GMSH/CAM:NAV/AXIS!
 ;   [symbol]
 ; 
 ; NAV/AXIS! names a compiled function:
 ;   Lambda-list: (C AXIS &OPTIONAL (STP 1.0))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/CAM:CAM (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL
 ;                   SINGLE-FLOAT)
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:nav/near!`
```
 ; GMSH/CAM:NAV/NEAR!
 ;   [symbol]
 ; 
 ; NAV/NEAR! names a compiled function:
 ;   Lambda-list: (C VAL &OPTIONAL (MODE (@PROJ-MODE C)))
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM SINGLE-FLOAT &OPTIONAL KEYWORD)
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Documentation:
 ;     change near value of current projection mode.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:nav/s!`
```
 ; GMSH/CAM:NAV/S!
 ;   [symbol]
 ; 
 ; NAV/S! names a compiled function:
 ;   Lambda-list: (C VAL &OPTIONAL (MODE (@PROJ-MODE C)))
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM SINGLE-FLOAT &OPTIONAL KEYWORD)
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Documentation:
 ;     change s value of current projection mode.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:nav/trans!`
```
 ; GMSH/CAM:NAV/TRANS!
 ;   [symbol]
 ; 
 ; NAV/TRANS! names a compiled function:
 ;   Lambda-list: (C AX VAL)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM KEYWORD SINGLE-FLOAT) *)
 ;   Documentation:
 ;     translate cam.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:pm`
```
 ; GMSH/CAM:PM
 ;   [symbol]
 ; 
 ; PM names a compiled function:
 ;   Lambda-list: (C &OPTIONAL (MODE (@PROJ-MODE C)))
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM &OPTIONAL KEYWORD)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     projection matrix. compatible with gmsh/scene and gmsh/xrend.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:scale-from-to!`
```
:missing:

 ; GMSH/CAM:SCALE-FROM-TO!
 ;   [symbol]
 ; 
 ; SCALE-FROM-TO! names a compiled function:
 ;   Lambda-list: (C FROM TO)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/CAM:CAM (UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32))
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:set-nav-mode`
```
 ; GMSH/CAM:SET-NAV-MODE
 ;   [symbol]
 ; 
 ; SET-NAV-MODE names a compiled function:
 ;   Lambda-list: (C MODE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM KEYWORD)
 ;                  (VALUES KEYWORD &OPTIONAL))
 ;   Documentation:
 ;     set nav mode :pan / :fly / :around.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:set-proj-mode`
```
 ; GMSH/CAM:SET-PROJ-MODE
 ;   [symbol]
 ; 
 ; SET-PROJ-MODE names a compiled function:
 ;   Lambda-list: (C MODE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM KEYWORD)
 ;                  (VALUES KEYWORD &OPTIONAL))
 ;   Documentation:
 ;     set nav mode. :ortho / :persp.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:update!`
```
 ; GMSH/CAM:UPDATE!
 ;   [symbol]
 ; 
 ; UPDATE! names a compiled function:
 ;   Lambda-list: (C &KEY UP POS VPN LOOK)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/CAM:CAM &KEY (:UP T) (:POS T) (:VPN T)
 ;                   (:LOOK T))
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Documentation:
 ;     consistently update camera values. use vpn to set view plane normal (away
 ;     from dop); or look to set view plane normal relative to position.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:vm`
```
 ; GMSH/CAM:VM
 ;   [symbol]
 ; 
 ; VM names a compiled function:
 ;   Lambda-list: (C)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     view matrix, compatible with gmsh/scene and gmsh/xrend.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

