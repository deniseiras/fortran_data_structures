! A simple generic linked list test program
program Unity_tests

  use Modlist
  use Moddata

  implicit none

  logical all_tests_passed 
  
  all_tests_passed = .true.
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


    logical function test_list_init() result(test_result)

      type(list_t), pointer :: ll => null()
      type(data_t), target :: dat_a
      type(data_ptr) :: ptr

      print*, ">>>>> Running Test List Initilize"
      test_result = .true.

      ! Initialize two data objects
      dat_a%x = 17.5

      ! Initialize the list with dat_a
      ptr%p => dat_a
      call list_init(ll, DATA=transfer(ptr, list_data))
      print *, 'Initializing list with data:', ptr%p

      ! Test the head node
      ptr = transfer(list_get(ll), ptr)
      if (ptr%p%x .ne. 17.5) then
        print *, 'Head node data should be: 17.5 but was', ptr%p%x
        test_result = .false.
      endif
      
      call list_free(ll)
      return

    end function



    logical function test_list_insert_second() result(test_result)

      type(list_t), pointer :: ll => null()
      type(data_t), target :: dat_a
      type(data_t), target :: dat_b
      type(data_ptr) :: ptr

      print*, ">>>>> Running Test List Insert second element"
      test_result = .true.

      ! Initialize two data objects
      dat_a%x = 17.5
      dat_b%x = 3.0

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
      if (ptr%p%x .ne. 17.5) then
        print *, 'Head node data should be: 17.5 but was', ptr%p%x
        call list_free(ll)
        test_result = .false.
        return
      endif
      
      ! Test the next node
      ptr = transfer(list_get(list_next(ll)), ptr)
      if (ptr%p%x .ne. 3.0) then
        print *, 'Second node data should be: 3.0 but was', ptr%p%x
        call list_free(ll)
        test_result = .false.
        return
      endif

      ! Free the list
      call list_free(ll)

    end function

  
end program Unity_tests