(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func (result i64)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func))
  (func (;0;) (type 1) (result i64)
    (local i32 i32 i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 0
    global.set 0
    local.get 0
    i32.const 8
    i32.add
    local.set 2
    i32.const 1048577
    i32.load8_u
    drop
    block  ;; label = @1
      block (result i32)  ;; label = @2
        i32.const 1048580
        i32.load
        local.tee 1
        i32.const 3
        i32.add
        local.tee 3
        local.get 1
        i32.ge_u
        if  ;; label = @3
          local.get 3
          i32.const -4
          i32.and
          local.tee 1
          i32.const 1388
          i32.add
          local.tee 3
          i32.const 1048584
          i32.load
          i32.gt_u
          if  ;; label = @4
            i32.const 1388
            i32.const 4
            call 1
            br 2 (;@2;)
          end
          i32.const 1048580
          local.get 3
          i32.store
          local.get 1
          br 1 (;@2;)
        end
        unreachable
      end
      local.tee 1
      if  ;; label = @2
        local.get 2
        i32.const 347
        i32.store offset=4
        local.get 2
        local.get 1
        i32.store
        br 1 (;@1;)
      end
      i32.const 4
      i32.const 1388
      i32.const 1048588
      i32.load
      local.tee 0
      i32.const 1
      local.get 0
      select
      call_indirect (type 0)
      unreachable
    end
    local.get 0
    i32.const 0
    i32.store offset=24
    local.get 0
    local.get 0
    i64.load offset=8
    i64.store offset=16
    local.get 0
    i32.const 16
    i32.add
    local.tee 2
    i32.load offset=4
    if  ;; label = @1
      local.get 2
      i32.load
      drop
    end
    local.get 0
    i32.const 32
    i32.add
    global.set 0
    i64.const 2)
  (func (;1;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    local.get 1
    i32.const 1048580
    i32.load
    local.tee 2
    i32.add
    i32.const 1
    i32.sub
    local.tee 4
    i32.const 0
    local.get 1
    i32.sub
    i32.and
    local.tee 5
    local.get 0
    i32.add
    local.set 3
    local.get 0
    i32.const 65535
    i32.add
    local.tee 6
    i32.const 16
    i32.shr_u
    local.set 7
    local.get 0
    i32.const 65536
    i32.add
    local.get 0
    i32.lt_u
    local.set 8
    local.get 2
    local.get 6
    i32.const -65536
    i32.and
    i32.add
    local.set 0
    local.get 2
    local.get 4
    i32.gt_u
    local.set 2
    block  ;; label = @1
      loop  ;; label = @2
        local.get 8
        br_if 1 (;@1;)
        local.get 7
        memory.grow
        i32.const -1
        i32.eq
        br_if 1 (;@1;)
        i32.const 1048584
        local.get 0
        i32.store
        local.get 1
        i32.eqz
        local.get 2
        i32.or
        br_if 1 (;@1;)
        local.get 0
        local.get 3
        i32.lt_u
        br_if 0 (;@2;)
      end
      i32.const 1048580
      local.get 3
      i32.store
      local.get 5
      return
    end
    unreachable)
  (func (;2;) (type 3)
    nop)
  (func (;3;) (type 0) (param i32 i32)
    i32.const 1048576
    i32.load8_u
    if  ;; label = @1
      unreachable
    end)
  (table (;0;) 2 2 funcref)
  (memory (;0;) 17)
  (global (;0;) (mut i32) (i32.const 1048576))
  (global (;1;) i32 (i32.const 1048592))
  (global (;2;) i32 (i32.const 1048592))
  (export "memory" (memory 0))
  (export "sum" (func 0))
  (export "_" (func 2))
  (export "__data_end" (global 1))
  (export "__heap_base" (global 2))
  (elem (;0;) (i32.const 1) func 3))
