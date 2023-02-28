! A simple generic linked list test program
program Unity_tests

  use Modlist
  use Moddata

  implicit none

  logical all_tests_passed 
  
  print*, ">>>>> Running Unity Tests"

  all_tests_passed = .true.
  all_tests_passed = all_tests_passed .and. test_list_init_empty()
  all_tests_passed = all_tests_passed .and. test_list_init()
  all_tests_passed = all_tests_passed .and. test_list_insert_second()

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

      print*, ">>>>> Running Test List Initilize Empty"
      test_result = .true.

      call list_init(ll)
      print *, 'Initializing empty list '

      ! Test the head node
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
      type(data_ptr) :: ptr

      print*, ">>>>> Running Test List Initilize"
      test_result = .true.

      ! Initialize two data objects
      dat_a%x = 2

      ! Initialize the list with dat_a
      ptr%p => dat_a
      call list_init(ll, DATA=transfer(ptr, list_data))
      print *, 'Initializing list with data:', ptr%p

      ! Test the head node
      ptr = transfer(list_get(ll), ptr)
      if (ptr%p%x .ne. 1) then
        print *, 'Head node data should be: 1 but was', ptr%p%x
        test_result = .false.
      endif
      
      call list_free(ll)
      return

    end function



    logical function test_list_insert_second() result(test_result)

      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), target :: dat_a
      type(data_t), target :: dat_b
      type(data_ptr) :: ptr

      print*, ">>>>> Running Test List Insert second element"
      test_result = .true.

      ! Initialize two data objects
      dat_a%x = 1
      dat_b%x = 2

      ! Initialize the list with dat_a
      ptr%p => dat_a
      call list_init(ll, DATA=transfer(ptr, list_data))
      print *, 'Initializing list with data:', ptr%p

      ! Insert dat_b into the list
      ptr%p => dat_b
      call list_insert(ll, DATA=transfer(ptr, list_data))
      print *, 'Inserting node with data:', ptr%p

      ! Test the head node
      ptr = transfer(list_get(ll), ptr)
      if (ptr%p%x .ne. 1) then
        print *, 'Head node data should be: 1 but was', ptr%p%x
        call list_free(ll)
        test_result = .false.
        return
      endif
      
      ! Test the next node
      ptr = transfer(list_get(list_next(ll)), ptr)
      if (ptr%p%x .ne. 2) then
        print *, 'Second node data should be: 2 but was', ptr%p%x
        call list_free(ll)
        test_result = .false.
        return
      endif

      ! Free the list
      call list_free(ll)

    end function

  
end program Unity_tests