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
 ;   Lambda-list: (SC BVH &KEY (PAR T) (SIZE 1000) (BS 1) (AA 1)
 ;                 (RAYLEN 2000.0) (MAX-DEPTH 13) (AO-REP 4) (VOL NIL)
 ;                 (VMULT 64.0) (VEXPT 0.3) (VDST 600.0) (VLIM 0.01)
 ;                 (VREC 7)
 ;                 (VDEPTH
 ;                  (IF VOL
 ;                      1
 ;                      0))
 ;                 (MISS BGK) (WORLD MISS) (RAYLEN2 (* 0.5 RAYLEN))
 ;                 (AO-MULT (FF (/ AO-REP))))
 ;   Derived type: (FUNCTION
 ;                  (GMSH/SCENE:SCENE GMSH/BVH:BVH &KEY (:PAR BOOLEAN)
 ;                   (:SIZE (UNSIGNED-BYTE 32)) (:BS (UNSIGNED-BYTE 32))
 ;                   (:AA (UNSIGNED-BYTE 32)) (:RAYLEN SINGLE-FLOAT)
 ;                   (:MAX-DEPTH (UNSIGNED-BYTE 32))
 ;                   (:AO-REP (UNSIGNED-BYTE 32)) (:VOL BOOLEAN)
 ;                   (:VMULT SINGLE-FLOAT) (:VEXPT SINGLE-FLOAT)
 ;                   (:VDST SINGLE-FLOAT) (:VLIM SINGLE-FLOAT)
 ;                   (:VREC (UNSIGNED-BYTE 32))
 ;                   (:VDEPTH (UNSIGNED-BYTE 32)) (:MISS KEYWORD)
 ;                   (:WORLD KEYWORD) (:RAYLEN2 SINGLE-FLOAT)
 ;                   (:AO-MULT SINGLE-FLOAT))
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
 ;      - vexpt  : volume light distance falloff. lower is brighter.
 ;      - vdepth : disable volume sampling after vdepth ray bounces.
 ;      - vrec   : max recursive depth in volume sampling. higher is more detailed.
 ;      - vlim   : volume sampling min recursive sampling step size.
 ;                 lower is more detailed. [0.0, 1.0]
 ;      - vol    : enable disable volumetric light. [:ll mat]
 ;      - par    : use parallelism.
 ; 
 ;   Source file: /data/x/gmsh/src/gmsh/cpu/render.lisp
```

