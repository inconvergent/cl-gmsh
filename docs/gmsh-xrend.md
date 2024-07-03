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
 ;   Lambda-list: (K &KEY (CONTEXT (FUNCTION XREND-WORKER-CONTEXT)))
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render-utils.lisp
```

## `gmsh/xrend:iter-timer`
```
:missing:

 ; GMSH/XREND:ITER-TIMER
 ;   [symbol]
```

## `gmsh/xrend:xrend`
```
 ; GMSH/XREND:XREND
 ;   [symbol]
 ; 
 ; XREND names a compiled function:
 ;   Lambda-list: (SC BVH &KEY (PAR T) (SIZE 1000) (BS 1) (AA 1) (VOL NIL)
 ;                 (RAYLEN 2000.0) (RAYLEN2 (* 0.5 RAYLEN)) (MAX-DEPTH 12)
 ;                 (AO-REP 4) (VMULT 64.0) (VDST 1000.0) (VLIM 100.0)
 ;                 (MISS BGK) (WORLD MISS) (AO-MULT (FF (/ AO-REP)))
 ;                 (AA-MULT (FF (/ AA))) (VLIM* (/ VLIM VDST))
 ;                 (VDEPTH
 ;                  (IF VOL
 ;                      1
 ;                      0)))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE GMSH/BVH:BVH &KEY (:PAR BOOLEAN)
 ;                   (:SIZE (UNSIGNED-BYTE 32)) (:BS (UNSIGNED-BYTE 32))
 ;                   (:AA (UNSIGNED-BYTE 32)) (:VOL BOOLEAN)
 ;                   (:RAYLEN SINGLE-FLOAT) (:RAYLEN2 SINGLE-FLOAT)
 ;                   (:MAX-DEPTH (UNSIGNED-BYTE 32))
 ;                   (:AO-REP (UNSIGNED-BYTE 32)) (:VMULT SINGLE-FLOAT)
 ;                   (:VDST SINGLE-FLOAT) (:VLIM SINGLE-FLOAT)
 ;                   (:MISS KEYWORD) (:WORLD KEYWORD)
 ;                   (:AO-MULT SINGLE-FLOAT) (:AA-MULT T)
 ;                   (:VLIM* SINGLE-FLOAT) (:VDEPTH (UNSIGNED-BYTE 32)))
 ;                  (VALUES NULL &OPTIONAL))
 ;   Documentation:
 ;     render scene from this scene/bvh.
 ;     
 ;     use (gmsh/xrend:init 4) before calling render to initialize parallel cores:
 ;     
 ;     keywords:
 ;      - aa     : rendering fidelity. higher is slower.
 ;      - raylen : ray length for sampling. not volume sampling.
 ;      - vdst   : ray length for sampling volume light. [:ll mat]
 ;      - vmult  : volume light brightness. [:ll mat]
 ;      - vdepth : disable volume sampling after vdepth ray bounces.
 ;      - vlim   : volume sampling min recursive sampling step size.
 ;                 lower is more detailed.
 ;      - vol    : enable disable volumetric light. [:ll mat]
 ;      - par    : use parallelism.
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render.lisp
```

