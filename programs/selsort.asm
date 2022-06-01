BEGIN CONSTANTS

SYS_REGION_SIZE, 1, 10
STACK_SIZE, 1, 1000 

END CONSTANTS 

# Stack Operations
        
BEGIN MACRO push, 1
        add r2, r1, r1
        sto args[0], r1
END MACRO


BEGIN MACRO pop, 1
        lod r1, args[0]
        sub r2, r1, r1
END MACRO


BEGIN CODE

start:
        put SYS_REGION_SIZE, r0 # for storing system info such as stack address
        mal r0, r0
        put STACK_SIZE, r2
        mal r2, r1
        sto r1, r0
        add r2, r1, r1
        put -1, r2

main:
        put &x, r4
        cal selsort
        hlt
        

selsort:
        put 0, r5 # r5 is outer index
        add r2, n, r12 # r12 = n - 1
        sub r12, r5, r9
        brn r9, outerloop
        brn r2, end_outerloop

outerloop:
        add r5, r4, r6
        lod r6, r7 # r7 is min value
        put 0, r8
        add r8, r7, r13
        add r8, r5, r8
        sub r2, r5, r9 # r9 = r5 + 1 = inner index

        sub n, r9, r10
        brn r10, innerloop
        brn r2, end_innerloop

innerloop:
        add r9, r4, r10
        lod r10, r10
        sub r7, r10, r11
        brn r11, newmin
        brn r2, nextiter
newmin:
        put 0, r11
        add r11, r9, r8
        add r11, r10, r7
nextiter:
        sub r2, r9, r9
        sub n, r9, r10
        brn r10, innerloop

end_innerloop:
        add r8, r4, r10
        sto r13, r10
        sto r7, r6

        sub r2, r5, r5
        sub r12, r5, r9
        brn r9, outerloop

end_outerloop:
        ret
END CODE        
