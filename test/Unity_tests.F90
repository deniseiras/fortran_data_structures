! A simple generic linked list test program
program Unity_tests

  use Modlist
  use Moddata

  implicit none

  logical all_tests_passed 
  
  print*, ""
  print*, ">>>>> Running Unity Tests"

  all_tests_passed = .true.
  all_tests_passed = all_tests_passed .and. test_list_init()
  all_tests_passed = all_tests_passed .and. test_list_insert_second()
  
  ! ToDo. Vale a pena deixar a lista usando inteiro puramente (sem tipo list_t) ?
  ! all_tests_passed = all_tests_passed .and. test_list_init_with_integer()
  ! all_tests_passed = all_tests_passed .and. test_list_insert_second_with_integer()

  if (all_tests_passed) then
    print*, ">>>>> All tests OK !"
    call exit(0)
  else
    print*, ">>>>> Some Tests failed!"
    call exit(-1)
  endif


  contains 


    logical function test_list_init() result(test_result)

      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), allocatable :: dat_a
      type(data_t) :: dat_test

      print*, ""
      print*, ">>>>> Running Test List Initilize"
      test_result = .true.

      allocate(dat_a)
      dat_a%x = 1

      call list_init(ll, dat_a)
      print *, 'Initializing list with data:', dat_a

      print *, 'Testing head node'      
      dat_test = list_get(ll)
      if (dat_test%x .ne. 1) then
        print *, 'Head node data should be: 1 but was', dat_test%x
        test_result = .false.
      endif
      
      call list_free(ll)
      return

    end function


    logical function test_list_insert_second() result(test_result)
      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), allocatable :: dat_x
      type(data_t) :: dat_test

      print*, ""
      print*, ">>>>> Running Test List Insert second element"
      test_result = .true.

      allocate(dat_x)
      dat_x%x = 1
      call list_init(ll, dat_x)
      print *, 'Initializing list with data:', dat_x%x
      deallocate(dat_x)

      allocate(dat_x)
      dat_x%x = 2
      call list_insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%x
      deallocate(dat_x)

      print *, 'Testing head node'      
      dat_test = list_get(ll)
      if (dat_test%x .ne. 2) then
        print *, 'Head node data should be: 2 but was', dat_test%x
        call list_free(ll)
        test_result = .false.
        return
      endif
      
      print *, 'Testing second node'
      dat_test = list_get(list_next(ll))
      if (dat_test%x .ne. 1) then
        print *, 'Second node data should be: 1 but was', dat_test%x
        call list_free(ll)
        test_result = .false.
        return
      endif

      ! Free the list
      call list_free(ll)

    end function


    ! logical function test_list_insert_second_with_integer() result(test_result)
    !   implicit none 

    !   type(list_t), pointer :: ll => null()
    !   integer, target :: dat_a(1)
    !   integer, pointer :: ptr

    !   print*, ""
    !   print*, ">>>>> Running Test List Insert second element with integer"
    !   test_result = .true.

    !   dat_a(1) = 1
    !   print *, 'Initializing list with data:', dat_a
    !   call list_init(ll, DATA=dat_a)

    !   dat_a(1) = 2
    !   print *, 'Inserting node with data:', dat_a     
    !   call list_insert(ll, DATA=dat_a)

    !   print *, 'Testing head node'
    !   ptr = transfer(list_get(ll), ptr)
    !   if (ptr .ne. 1) then
    !     print *, 'Head node data should be: 1 but was', ptr
    !     test_result = .false.
    !     call list_free(ll)
    !     return
    !   endif

    !   print *, 'Testing second node'
    !   ptr = transfer(list_get(list_next(ll)), ptr)
    !   if (ptr .ne. 2) then
    !     print *, 'Head node data should be: 2 but was', ptr
    !     test_result = .false.
    !     call list_free(ll)
    !     return
    !   endif
      
    !   call list_free(ll)
    !   return

    ! end function
  


    ! logical function test_list_init_with_integer() result(test_result)
    !   implicit none 

    !   type(list_t), pointer :: ll => null()
    !   integer, target :: dat_a(1)
    !   integer, pointer :: ptr

    !   print*, ""
    !   print*, ">>>>> Running Test List Initilize with integer"
    !   test_result = .true.

    !   dat_a(1) = 1
    !   call list_init(ll, DATA=dat_a)
    !   print *, 'Initializing list with data:', dat_a
    
    !   ptr = transfer(list_get(ll), ptr)
    !   print *, 'Testing head node'
    !   if (ptr .ne. 1) then
    !     print *, 'Head node data should be: 1 but was', ptr
    !     test_result = .false.
    !   endif

    !   call list_free(ll)
    !   return

    ! end function


end program Unity_tests