BEGIN INCLUDES
        include "harness.asm"
END INCLUDES
        
BEGIN CODE

main:
        put &x, r4 # selsort takes single argument, a pointer passed in r4
        cal selsort
        exit 0 # exit with status code 0 (which signifies all is good)

     
###############################################################################
# Selection Sort (function dirties r5-r13 (i.e.not saved/restored from stack))
#        
# arguments: r4 - address of array of integers to sort of length n (reg n)
# sorts array pointed to by r4 in-place        
###############################################################################


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
