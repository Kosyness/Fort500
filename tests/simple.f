integer x, y, z
x = 1
y = 0
z = 2

if (x) then
    write "x is positive"
endif

if (y) then
    write "this should not be called"
endif

if (z) then
    write "this should be called again though"
endif