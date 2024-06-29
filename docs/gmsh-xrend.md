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

## `gmsh/xrend:xrend`
```
 ; GMSH/XREND:XREND
 ;   [symbol]
 ; 
 ; XREND names a compiled function:
 ;   Lambda-list: (SC BVH &KEY (PAR T) (SIZE 1000) (BS 1) (AA 1) (VOL NIL)
 ;                 (RAYLEN 2000.0) (RAYLEN2 (* 0.5 RAYLEN)) (MAX-DEPTH 12)
 ;                 (AO-REP 4) (VMULT 64.0) (VEXPT 1.45) (VDST 1000.0)
 ;                 (VLIM 0.01) (VREC 6) (MISS BGK) (WORLD MISS)
 ;                 (AO-MULT (FF (/ AO-REP))) (AA-MULT (FF (/ AA)))
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
 ;                   (:VEXPT SINGLE-FLOAT) (:VDST SINGLE-FLOAT)
 ;                   (:VLIM SINGLE-FLOAT) (:VREC (UNSIGNED-BYTE 32))
 ;                   (:MISS KEYWORD) (:WORLD KEYWORD)
 ;                   (:AO-MULT SINGLE-FLOAT) (:AA-MULT T)
 ;                   (:VDEPTH (UNSIGNED-BYTE 32)))
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
 ;      - vexpt  : volume light distance falloff. higher is brighter.
 ;      - vdepth : disable volume sampling after vdepth ray bounces.
 ;      - vrec   : max recursive depth in volume sampling. higher is more detailed.
 ;      - vlim   : volume sampling min recursive sampling step size.
 ;                 lower is more detailed. [0.0, 1.0]
 ;      - vol    : enable disable volumetric light. [:ll mat]
 ;      - par    : use parallelism.
 ;     
 ;       TODO: ; ve = 1.2 -> less fog; ve = 0.3 -> more
 ; 
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render.lisp
```

