## `gmsh/cam:@pos`
```
 ; GMSH/CAM:@POS
 ;   [symbol]
 ; 
 ; @POS names a compiled function:
 ;   Lambda-list: (PROJ)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get pos.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@s`
```
 ; GMSH/CAM:@S
 ;   [symbol]
 ; 
 ; @S names a compiled function:
 ;   Lambda-list: (PROJ)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get scale.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@up`
```
 ; GMSH/CAM:@UP
 ;   [symbol]
 ; 
 ; @UP names a compiled function:
 ;   Lambda-list: (PROJ)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get up.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@vpn`
```
 ; GMSH/CAM:@VPN
 ;   [symbol]
 ; 
 ; @VPN names a compiled function:
 ;   Lambda-list: (PROJ)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get vpn.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:@xy`
```
 ; GMSH/CAM:@XY
 ;   [symbol]
 ; 
 ; @XY names a compiled function:
 ;   Lambda-list: (PROJ)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get view plane offset.
 ;   Inline proclamation: INLINE (inline expansion available)
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
 ;     GMSH/CAM::VPN
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::UP
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::POS
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::U
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::V
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::SU
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::SV
 ;       Type: VEQ:FVEC
 ;       Initform: (GMSH/CAM::ZERO)
 ;     GMSH/CAM::XY
 ;       Type: VEQ:FVEC
 ;       Initform: (VEQ:F2$VAL 0.0)
 ;     GMSH/CAM::S
 ;       Type: VEQ:FF (unboxed)
 ;       Initform: 1.0
```

## `gmsh/cam:cam-pos`
```
:missing:

 ; GMSH/CAM:CAM-POS
 ;   [symbol]
 ; 
 ; CAM-POS names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF CAM-POS) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:cam-s`
```
:missing:

 ; GMSH/CAM:CAM-S
 ;   [symbol]
 ; 
 ; CAM-S names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF CAM-S) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION (SINGLE-FLOAT GMSH/CAM:CAM)
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:cam-u`
```
:missing:

 ; GMSH/CAM:CAM-U
 ;   [symbol]
 ; 
 ; CAM-U names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF CAM-U) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:cam-up`
```
:missing:

 ; GMSH/CAM:CAM-UP
 ;   [symbol]
 ; 
 ; CAM-UP names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF CAM-UP) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:cam-v`
```
:missing:

 ; GMSH/CAM:CAM-V
 ;   [symbol]
 ; 
 ; CAM-V names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF CAM-V) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:cam-vpn`
```
:missing:

 ; GMSH/CAM:CAM-VPN
 ;   [symbol]
 ; 
 ; CAM-VPN names a compiled function:
 ;   Lambda-list: (INSTANCE)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
 ; 
 ; (SETF CAM-VPN) names a compiled function:
 ;   Lambda-list: (VALUE INSTANCE)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) GMSH/CAM:CAM)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:export-data`
```
 ; GMSH/CAM:EXPORT-DATA
 ;   [symbol]
 ; 
 ; EXPORT-DATA names a compiled function:
 ;   Lambda-list: (P)
 ;   Derived type: (FUNCTION (GMSH/CAM:CAM) (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     export the neccessary values to recreate cam. import with gmsh/cam:import-data.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:import-data`
```
 ; GMSH/CAM:IMPORT-DATA
 ;   [symbol]
 ; 
 ; IMPORT-DATA names a compiled function:
 ;   Lambda-list: (P)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     recreate proj from an a list exported by gmsh/cam:export-data.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:make`
```
 ; GMSH/CAM:MAKE
 ;   [symbol]
 ; 
 ; MAKE names a compiled function:
 ;   Lambda-list: (&KEY (S 1.0) (POS (F3$VAL 1000.0)) VPN LOOK
 ;                 (UP (F3$POINT 0.0 0.0 1.0)) (XY (F2$VAL 500.0)))
 ;   Derived type: (FUNCTION
 ;                  (&KEY (:S SINGLE-FLOAT)
 ;                   (:POS (SIMPLE-ARRAY SINGLE-FLOAT)) (:VPN T) (:LOOK T)
 ;                   (:UP (SIMPLE-ARRAY SINGLE-FLOAT))
 ;                   (:XY (SIMPLE-ARRAY SINGLE-FLOAT)))
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Documentation:
 ;     make camera
 ;     
 ;       default up is (0 0 1)
 ;       default pos is (1000.0 1000.0 1000.0)
 ;       if look and vpn are unset, the camera will look at the origin.
 ;     
 ;       default scale is 1.0
 ;       default xy is (0.0 0.0)
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:pan-cam`
```
:missing:

 ; GMSH/CAM:PAN-CAM
 ;   [symbol]
```

## `gmsh/cam:pan-xy`
```
:missing:

 ; GMSH/CAM:PAN-XY
 ;   [symbol]
```

## `gmsh/cam:pm`
```
 ; GMSH/CAM:PM
 ;   [symbol]
 ; 
 ; PM names a compiled function:
 ;   Lambda-list: (P S &OPTIONAL (NEAR 0.1) (FAR 500.0))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/CAM:CAM SINGLE-FLOAT &OPTIONAL SINGLE-FLOAT
 ;                   SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     projection matrix. compatible with gmsh/scene
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:rotate`
```
:missing:

 ; GMSH/CAM:ROTATE
 ;   [symbol]
```

## `gmsh/cam:update`
```
 ; GMSH/CAM:UPDATE
 ;   [symbol]
 ; 
 ; UPDATE names a compiled function:
 ;   Lambda-list: (PROJ &KEY S XY UP POS VPN LOOK)
 ;   Derived type: (FUNCTION
 ;                  (GMSH/CAM:CAM &KEY (:S T) (:POS T) (:VPN T) (:LOOK T)
 ;                   (:UP T) (:XY T))
 ;                  (VALUES GMSH/CAM:CAM &OPTIONAL))
 ;   Documentation:
 ;     update projection parameters.
 ;     
 ;       use vpn to set view plane normal directly, or look to set view plane normal
 ;       relative to camera.
 ;     
 ;       ensures that internal state is updated appropriately.
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:vm`
```
 ; GMSH/CAM:VM
 ;   [symbol]
 ; 
 ; VM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %VM
 ;     ARGS: (P (VA 3 LOOK))
 ;     DOCSTRING: view matrix, compatible with gmsh/scene
 ;     defined via veq:FVDEF*
 ;   Source file: /data/x/gmsh/src/gmsh/cam.lisp
```

## `gmsh/cam:zoom`
```
:missing:

 ; GMSH/CAM:ZOOM
 ;   [symbol]
```

