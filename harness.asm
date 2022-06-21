BEGIN CONSTANTS

        SYS_REGION_SIZE, 1, 10
        STACK_SIZE, 1, 1000 

END CONSTANTS 

###############################################################################
# Data Section        
###############################################################################
        
BEGIN DATA

        # this contains the status code by convention outputted by the program
        status, 1, 0

END DATA

###############################################################################
# Useful Macros
###############################################################################

# Stack Operations
        
BEGIN MACRO push, 1
        add r2, r1, r1
        sto args[0], r1
END MACRO


BEGIN MACRO pop, 1
        lod r1, args[0]
        sub r2, r1, r1
END MACRO

        
BEGIN MACRO popx
        sub r2, r1, r1
END MACRO


# Takes a constant as argument and stores it at &status, then halts
BEGIN MACRO exit, 1
        put args[0], r4
        cal set_status
        hlt
END MACRO


###############################################################################
# Initialization Code
###############################################################################
        
BEGIN CODE
        
start:
        put SYS_REGION_SIZE, r0 # for storing system info such as stack address
        mal r0, r0
        put STACK_SIZE, r2
        mal r2, r1
        sto r1, r0
        add r2, r1, r1
        put -1, r2
        brn r2, harness_end # skip over function definitions


###############################################################################
# Useful Functions
###############################################################################
        

# single integer argument passed in r4
set_status:
        put &status, r5
        sto r4, r5
        ret

harness_end:    

END CODE
