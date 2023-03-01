! A simple generic linked list test program
program Unity_tests

  use Modlist
  use Moddata

  implicit none

  logical all_tests_passed 
  
  print*, ""
  print*, ">>>>> Running Unity Tests"

  all_tests_passed = .true.
  all_tests_passed = all_tests_passed .and. test_list_init_empty()
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


  logical function test_list_init_empty() result(test_result)

      implicit none 

      type(list_t), pointer :: ll => null()

      print*, ""
      print*, ">>>>> Running Test List Initilize Empty"
      test_result = .true.

      call list_init(ll)
      print *, 'Initializing empty list '

      print *, 'Testing empty list'
      if (associated(list_get(ll))) then
        print *, 'List head node should be empty!'
        test_result = .false.
      endif
      
      call list_free(ll)
      return

    end function


    logical function test_list_init() result(test_result)

      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), target :: dat_a
      type(data_t), pointer :: ptr

      print*, ""
      print*, ">>>>> Running Test List Initilize"
      test_result = .true.

      ! Initialize two data objects
      dat_a%x = 1

      ! Initialize the list with dat_a
      ptr => dat_a
      call list_init(ll, DATA=transfer(ptr, list_data))
      print *, 'Initializing list with data:', ptr

      ! Test the head node
      ptr = transfer(list_get(ll), ptr)
      if (ptr%x .ne. 1) then
        print *, 'Head node data should be: 1 but was', ptr%x
        test_result = .false.
      endif
      
      call list_free(ll)
      return

    end function



    !ToDo - Memory Invasion. If discommented, test test_list_insert_second_with_integer fails !
    logical function test_list_insert_second() result(test_result)
      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), target :: dat_a
      type(data_t), target :: dat_b
      type(data_t), pointer :: ptr

      print*, ""
      print*, ">>>>> Running Test List Insert second element"
      test_result = .true.

      dat_a%x = 1
      dat_b%x = 2

      ptr => dat_a
      call list_init(ll, DATA=transfer(ptr, list_data))
      print *, 'Initializing list with data:', ptr

      ptr => dat_b
      call list_insert(ll, DATA=transfer(ptr, list_data))
      print *, 'Inserting node with data:', ptr

      print *, 'Testing head node'      
      ptr = transfer(list_get(ll), ptr)
      if (ptr%x .ne. 1) then
        print *, 'Head node data should be: 1 but was', ptr%x
        call list_free(ll)
        test_result = .false.
        return
      endif
      
      print *, 'Testing second node'
      ptr = transfer(list_get(list_next(ll)), ptr)
      if (ptr%x .ne. 2) then
        print *, 'Second node data should be: 2 but was', ptr%x
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