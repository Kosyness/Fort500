integer x(10,100,2),z(2),i,j,N
real a logical ll,yy(10)
record integer i,j real a(10) record logical t,f endrec logic(10) endrec r
character _str_(2,10)
data x/2000/,z/2,-9/,N/0H3FC70/
data a/.314159e-31/,ll/.false./,yy/.false.,.true.,.true./
data r/5,3,7.0,9.0,3.0,-2.0,0.0,-4.5,2.0,0.0,1.0,-3.0/
data _str_/"string1", "string2"/
$ no comment
100 yy(z) = .not. ll .and. .not. x(10,i,g(j)).gt.y(z) .or. z .eq. 0B11
call aa
1000 if (b(i,j+k) .and. (x(i,j,g(i+j)) .gt. a(k**i))) stop
1001 continue
do i=1,x(10,g(y(i*j)+j),1),3
integer i1,j1,k1,m(100)
character c(2)
135 read (m(i1),((x(i1,j1,k1),i1=1,x(i,j,2)),j1=1,100),k1=1,N,2*m(i1)),z
if (yy(i)) goto 1000
c(1) = '\t'
r:a(x(r:j)+1) = -3.2
if (a.gt.0B.01.and.b(0,a*k**i).and.g(m(i)).lt.x(i,i+2,g(j))) then
   integer z,a(10)
   101 read z,a(z)
   105 if (z .gt. 0 .and. z .le. 6) goto z,(100,101,102,103,104,105)
   c(2) = '\n'
else
   integer i logical l(1000)
   do i=1,N,2
      1000 l(i) = x(i,g(i),g(j)).le.a
   enddo
   c(2) = 'n'
endif
102 call _try_me_(x(i,int(ll),z),m,r:logic(z):t)
103 if (y(g(y(z-2)))+a) 100,1000,1001
104 yy(m(i*j)) = .not. x(i,j,z) .ge. i
enddo $ 1000 $
end
subroutine _try_me_(integer n,a(n), logical l)
integer i
do i=1,n
   if (l .and. a(i) .gt. 0) a(i) = a(i) - i
enddo
return
end
real function y(integer n)
integer i,j
if (n .gt. 0) then y = n
else
   j = 0
   do i=1,n+1,n/2
      j = j+i
   enddo
   y = j
endif
return
end
integer function g(integer i)
g = i**5
return
end
integer function int(logical l)
if (l) then int = 1 else int = 0 endif
stop
end
subroutine www
write "Hello!"
return
end
subroutine aa
call www
end
integer function a(integer z)
integer i
read i
a = 0
if (b(i,z)) if (i) 100,200,300
300 return
200 write z
a = 1
end
logical function b(integer n)
b = .false.
if (n.eq.0H100) b = .true.
end
