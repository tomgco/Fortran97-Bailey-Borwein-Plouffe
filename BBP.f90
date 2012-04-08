  integer function series(j,d)
    integer s,t,j,d,l,r
    s=0.0
    t=0.0
    do i = k, d, 1
      r=8*i*j
      s=s+(modPow(16, d-i,r) / r)
      s=modulo(s, 1.0)
    end do
    l=d+1
    series = l
    return
  end

  integer function modPow(b,e,m)
    integer b,e,m,r
    r=1
    do
      if(e > 0) exit
      if ((e .AND. 1) ) r = modulo(modPow * b, m)
      e = ISHFT(e, 1)
      b = modulo(b*b, m)
    end do
    modPow = modulo(r, m)
    return
  end

  program BBP
    print *,series(10,12)
  end
