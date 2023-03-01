! A simple generic linked list test program
program Modular_tests

  use Modlist
  use Moddata

  implicit none

  logical all_tests_passed 

  print*, ">>>>> Running Modular Tests"
  
  all_tests_passed = .true.
  all_tests_passed = all_tests_passed .and. test_list_insert_performance()

  if (all_tests_passed) then
    print*, ">>>>> All tests OK !"
    call exit(0)
  else
    print*, ">>>>> Some Tests failed!"
    call exit(-1)
  endif


  contains 


    logical function test_list_insert_performance() result(test_result)

      type(list_t), pointer :: ll => null()
      type(list_t), pointer :: list_pointer => null()
      type(data_t), target  :: dat_a
      type(data_t), pointer :: data_t_ptr_aux

      integer :: an_integer
      integer, parameter :: p_max_iterations = 1000000
      integer, parameter :: p_insert_max_time_allowed = 1.0
      integer, parameter :: p_visit_max_time_allowed = 1.0
      real time_initial, time_final 


      print*, ">>>>> Running Test List Insert and Visit Performance"
      test_result = .true.

      call list_init(ll)

      ! List Insert test
      !
      call cpu_time(time_initial)
      do an_integer = 0, p_max_iterations
        dat_a%x = an_integer
        data_t_ptr_aux => dat_a
        call list_insert(ll, DATA=transfer(data_t_ptr_aux, list_data))
      enddo
      call cpu_time(time_final)
      
      ! check performance
      print *, 'Insert Max time = ', p_insert_max_time_allowed, '; Run time = ', time_final-time_initial
      if (time_final-time_initial > p_insert_max_time_allowed) then
        print *, 'Insert Max time exceeded!!!'
        test_result = .false.
        call list_free(ll)
        return
      endif

      ! check HEAD
      data_t_ptr_aux = transfer(list_get(list_next(ll)), data_t_ptr_aux)
      print*, 'Checking head element ...', data_t_ptr_aux%x
      if(data_t_ptr_aux%x .ne. p_max_iterations) then
        print *, 'Head element should be: ', p_max_iterations, ' but was', data_t_ptr_aux%x
        test_result = .false.
        call list_free(ll)
        return
      endif


      ! List Visit test
      call cpu_time(time_initial)
      list_pointer => list_next(ll)
      do 
        data_t_ptr_aux = transfer(list_get(list_pointer), data_t_ptr_aux)
        ! print*, data_t_ptr_aux%x
        list_pointer => list_next(list_pointer)
        if (.not. associated(list_pointer)) exit
      enddo
      call cpu_time(time_final)


      print*, 'Checking last element ...', data_t_ptr_aux%x
      if (data_t_ptr_aux%x .ne. 0) then
        print *, 'Last element should be 0 but was', data_t_ptr_aux%x
        test_result = .false.
        call list_free(ll)
        return
      endif


      print *, 'Visit Max time = ', p_visit_max_time_allowed, '; Run time = ', time_final-time_initial
      if (time_final-time_initial > p_visit_max_time_allowed) then
        print *, 'Visit Max time exceeded!!!'
        test_result = .false.
        call list_free(ll)
        return
      endif

      call list_free(ll)
      return

    end function

  
end program Modular_tests