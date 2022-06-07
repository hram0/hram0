import sys
import hram
import json
from hram import HRAM0


if __name__ == "__main__":
    mach = HRAM0(hram.HRAM0S_PARAMS)

    encode = lambda x: [int(s) for s in x.split(',') if len(s) > 0]
    decode = lambda M, n: M


    if len(sys.argv) < 2:
        print("usage: python3 %s <input-file>" % sys.argv[0])
        exit(1)

    in_filename = sys.argv[1]
    with open(in_filename) as in_f:
        obj = json.load(in_f)
        assert 'code' in obj
        code = obj['code']
        data = []
        if 'data' in obj:
            data = obj['data']

        prog = hram.Program(code, data, encode, decode)

        if not mach.validate_code(prog.code):
            print('Invalid program')
            exit(1)

        executor = hram.Executor(mach)
        x_str = input("Program Input (x) - comma-separated array of integers: ")
        x = encode(x_str)
        n = len(x)

        M = executor.execute(prog, x_str)

        s = input("Memory address (q to quit): ").strip()
        while s != 'q':
            if len(s) == 0 or not s.isdecimal():
                print('Invalid input')
            else:
                a = int(s)
                if a < 0 or a not in M:
                    print("Address %d is invalid" % a)
                else:
                    print(M[a])

            s = input("Memory address (q to quit): ").strip().lower()

    exit(0)
