## `gmsh/gl:bind-buffer-object`
```
:missing:

 ; GMSH/GL:BIND-BUFFER-OBJECT
 ;   [symbol]
 ; 
 ; BIND-BUFFER-OBJECT names a compiled function:
 ;   Lambda-list: (BUF TY O &KEY (SLOT 0) &AUX (ARR (TO-GL-ARRAY O TY)))
 ;   Derived type: (FUNCTION (T T T &KEY (:SLOT T)) *)
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:compile-shader`
```
 ; GMSH/GL:COMPILE-SHADER
 ;   [symbol]
 ; 
 ; COMPILE-SHADER names a compiled function:
 ;   Lambda-list: (SHADER-TYPE SRC &AUX (S (CREATE-SHADER SHADER-TYPE)))
 ;   Derived type: (FUNCTION (KEYWORD STRING) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     build shader of type :vertex-shader, :fragment-shader at src.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:init-controller`
```
 ; GMSH/GL:INIT-CONTROLLER
 ;   [symbol]
 ; 
 ; INIT-CONTROLLER names a macro:
 ;   Lambda-list: (CONTROLLERS)
 ;   Documentation:
 ;     initialize connected controllers. incomplete.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:init-viewport`
```
 ; GMSH/GL:INIT-VIEWPORT
 ;   [symbol]
 ; 
 ; INIT-VIEWPORT names a compiled function:
 ;   Lambda-list: (&KEY (W 1000) (H 1000) (V 0.0))
 ;   Derived type: (FUNCTION
 ;                  (&KEY (:W (UNSIGNED-BYTE 32)) (:H (UNSIGNED-BYTE 32))
 ;                   (:V SINGLE-FLOAT))
 ;                  (VALUES &OPTIONAL))
 ;   Documentation:
 ;     initialize gl viewport.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:make-buftex`
```
:missing:

 ; GMSH/GL:MAKE-BUFTEX
 ;   [symbol]
 ; 
 ; MAKE-BUFTEX names a compiled function:
 ;   Lambda-list: (P NAME BUF BUF-TEX ARR &KEY (SLOT 0) (LAYOUT RGBA32F))
 ;   Derived type: (FUNCTION (T T T T T &KEY (:SLOT T) (:LAYOUT T)) *)
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:make-program`
```
 ; GMSH/GL:MAKE-PROGRAM
 ;   [symbol]
 ; 
 ; MAKE-PROGRAM names a compiled function:
 ;   Lambda-list: (NAME &OPTIONAL
 ;                 (LOC (INTERNAL-PATH-STRING src/shaders/)))
 ;   Derived type: (FUNCTION (KEYWORD &OPTIONAL STRING)
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     make program from shader files (name.frag, name.vert) located at path loc.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:make-program*`
```
 ; GMSH/GL:MAKE-PROGRAM*
 ;   [symbol]
 ; 
 ; MAKE-PROGRAM* names a compiled function:
 ;   Lambda-list: (VS FS &AUX (P (CREATE-PROGRAM)))
 ;   Derived type: (FUNCTION (STRING STRING) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     make shader program from these two source codes.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:make-render`
```
:missing:

 ; GMSH/GL:MAKE-RENDER
 ;   [symbol]
 ; 
 ; MAKE-RENDER names a compiled function:
 ;   Lambda-list: (SC)
 ;   Derived type: (FUNCTION (GMSH/SCENE:SCENE)
 ;                  (VALUES FUNCTION FUNCTION FUNCTION FUNCTION &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:sdl-info`
```
 ; GMSH/GL:SDL-INFO
 ;   [symbol]
 ; 
 ; SDL-INFO names a compiled function:
 ;   Lambda-list: ()
 ;   Derived type: (FUNCTION NIL (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     print sdl info.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:set-uniform-f`
```
 ; GMSH/GL:SET-UNIFORM-F
 ;   [symbol]
 ; 
 ; SET-UNIFORM-F names a macro:
 ;   Lambda-list: (P NAME VAL)
 ;   Documentation:
 ;     set float uniform.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:set-uniform-i`
```
 ; GMSH/GL:SET-UNIFORM-I
 ;   [symbol]
 ; 
 ; SET-UNIFORM-I names a macro:
 ;   Lambda-list: (P NAME VAL)
 ;   Documentation:
 ;     set integer iniform.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:set-uniform-mat-4f`
```
 ; GMSH/GL:SET-UNIFORM-MAT-4F
 ;   [symbol]
 ; 
 ; SET-UNIFORM-MAT-4F names a macro:
 ;   Lambda-list: (P NAME VAL &OPTIONAL (TRANSPOSE T))
 ;   Documentation:
 ;     set 4f uniform.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:show-diag`
```
:missing:

 ; GMSH/GL:SHOW-DIAG
 ;   [symbol]
 ; 
 ; SHOW-DIAG names a compiled function:
 ;   Lambda-list: (ITT T0)
 ;   Derived type: (FUNCTION ((UNSIGNED-BYTE 32) T)
 ;                  (VALUES NULL &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:tick`
```
 ; GMSH/GL:TICK
 ;   [symbol]
 ; 
 ; TICK names a compiled function:
 ;   Lambda-list: (&OPTIONAL (S 1000.0))
 ;   Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get current tick.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:to-gl-array`
```
 ; GMSH/GL:TO-GL-ARRAY
 ;   [symbol]
 ; 
 ; TO-GL-ARRAY names a compiled function:
 ;   Lambda-list: (SEQ TYPE &AUX (L (LENGTH SEQ))
 ;                 (ARR (ALLOC-GL-ARRAY TYPE L)))
 ;   Derived type: (FUNCTION (SEQUENCE KEYWORD) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     create gl array from seq.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

## `gmsh/gl:window-context`
```
 ; GMSH/GL:WINDOW-CONTEXT
 ;   [symbol]
 ; 
 ; WINDOW-CONTEXT names a macro:
 ;   Lambda-list: ((W H &KEY (WIN (QUOTE WIN)) (CTXT (QUOTE CTXT))
 ;                  (CONTROLLERS (QUOTE CONTROLLERS)) (RUN (QUOTE RUN)))
 ;                 &BODY BODY)
 ;   Documentation:
 ;     create gl window context. see basic.lisp for example use. incomplete.
 ;   Source file: /data/x/gmsh/src/gmsh/gpu/gl.lisp
```

