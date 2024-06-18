## `gmsh/xrend:get-info-fx`
```
:missing:

 ; GMSH/XREND:GET-INFO-FX
 ;   [symbol]
 ; 
 ; GET-INFO-FX names a compiled function:
 ;   Lambda-list: (SIZE AA)
 ;   Derived type: (FUNCTION ((UNSIGNED-BYTE 32) (UNSIGNED-BYTE 32))
 ;                  (VALUES FUNCTION &OPTIONAL))
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render-utils.lisp
```

## `gmsh/xrend:init`
```
:missing:

 ; GMSH/XREND:INIT
 ;   [symbol]
 ; 
 ; INIT names a macro:
 ;   Lambda-list: (K &KEY (CONTEXT (FUNCTION XREND-WORKER-CONTEXT))
 ;                 (BINDINGS (QUOTE ((*STCK* MAKE-FAST-STACK N 2048)))))
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render-utils.lisp
```

## `gmsh/xrend:iter-timer`
```
:missing:

 ; GMSH/XREND:ITER-TIMER
 ;   [symbol]
```

## `gmsh/xrend:render`
```
 ; GMSH/XREND:RENDER
 ;   [symbol]
 ; 
 ; RENDER names a compiled function:
 ;   Lambda-list: (SC BVH &KEY (AA 1) (PAR T) (SIZE 1000) (BS 1)
 ;                 (RAYLEN 2000.0))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE GMSH/BVH:BVH &KEY
 ;                   (:AA (UNSIGNED-BYTE 32)) (:PAR BOOLEAN)
 ;                   (:SIZE (UNSIGNED-BYTE 32)) (:BS (UNSIGNED-BYTE 32))
 ;                   (:RAYLEN SINGLE-FLOAT))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     render scene from this scene/bvh.
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render.lisp
```

