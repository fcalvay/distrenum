      program distri
      parameter(n=13)
      parameter(nconf=300)
      integer,dimension(0:n) :: itboucle
      integer,dimension(0:n,nconf) :: exact
      integer,dimension(nconf) :: punch
      integer,dimension(:),allocatable::energ,ni
      in=0
      itboucle=0
       do while (itboucle(n)<2) 
          itboucle(0)=itboucle(0)+1
          do i=0,n
            if(itboucle(i).gt.n) then
               itboucle(i)=0
               itboucle(i+1)=itboucle(i+1)+1
            endif
          enddo
           isuc=isucces(itboucle,n)
           if(isuc==1) then
             in=in+1
            do j=0 ,n
              exact(j,in)=itboucle(j)
            enddo
         endif
        enddo
        write(6,*) 'fin de generation des exacts'
      punch=0
      inconf=in
       write(6,*) in
      idum=2002                 ! seed for random number generator
      nt=200000
      allocate(ni(0:n),energ(0:n))
      do i=0,n
          energ(i)=i
      enddo
      punch=0
      do it=1,nt
20           continue
             ni=0
             j=-1
             nsum=0
             isum=0
30           continue
          if(j.lt.n) then
              j=j+1
        else
           goto 20
        endif
            ni(j)=n*ran0(idum)
            isum=isum+ni(j)*energ(j)
            nsum=nsum+ni(j)
        if((nsum.lt.n).or.(isum.lt.n))  goto 30
        if((nsum.gt.n).or.(isum.gt.n))  goto 20
          do iex=1,inconf
             iflag=1
              do i=1,n
                if((ni(i)-exact(i,iex)).ne.0) then
                   iflag=iflag*0
                endif
             enddo
           if(iflag==1) then
                punch(iex)=punch(iex)+1
             endif
          enddo
       enddo
       do i=1,inconf
           write(6,*) exact(:,i),punch(i)/inconf
       enddo

      stop
      end
      
!
!---------------------------------
      function ran0(idum)
!---------------------------------
!
! completely basic (and wrong) random number generator
! replace with your own !
!
      idum=123456789*idum+987654321
      idum=mod(idum,2**30)
      ran0=idum/(2**30*1.0)
      if(ran0.lt.0.0) ran0=ran0+1.0
      return
      end





!---------------------------------
      integer function nen(tab,n) 
      integer :: res,tab(0:n)
!---------------------------------
      res=0
      do i=0,n
        res=res+i*tab(i)
        if(res>n) then
           res=n+1
           goto 10
         endif
      enddo 
10    continue
      nen=res
      return
      end
!---------------------------------
      integer function nn(tab,n) 
      integer :: res,tab(0:n)
!---------------------------------
      res=0
      do i=0,n
        if(res>n) then
           res=n+1
           goto 10
         endif
        res=res+tab(i)
      enddo 
10    continue
      nn=res
      return
      end
!---------------------------------
      integer function isucces(nttry,n) 
      integer :: nttry(0:n)
!---------------------------------
         isucces=0
         netry=nen(nttry,n)
         if(netry==n) then
         ntry=nn(nttry,n)
         if(ntry==n) then
!           isucces
           isucces=1
           iweight=ifact(n)
           do j=0,n
              iweight=iweight/ifact(nttry(j))
           enddo
           write(6,*) nttry,iweight
          endif
          endif
          
          return
          end


!---------------------------------
      integer function ifact(n) 
      integer :: res,n
!---------------------------------
      res=1
      do i=1,n
        res=res*i
      enddo 
      ifact=res
      return
      end




