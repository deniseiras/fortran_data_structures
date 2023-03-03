! A simple generic linked list test program
program Modular_tests

  use Modlist
  use Moddata

  implicit none

  logical all_tests_passed 

  print*, ">>>>> Running Modular Tests"
  
  all_tests_passed = .true.
  all_tests_passed = all_tests_passed .and. test_list_insert_performance()
  all_tests_passed = all_tests_passed .and. test_list_random_remove_performance()

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
      integer, parameter :: p_num_of_insertions = 1000000
      real, parameter :: p_insert_max_time_allowed = 0.2
      real, parameter :: p_visit_max_time_allowed = 0.1
      real time_initial, time_final 


      print*, ">>>>> Running Test List Insert and Visit Performance"
      test_result = .true.

      call list_init(ll)

      ! List Insert test
      !
      call cpu_time(time_initial)
      do an_integer = 0, p_num_of_insertions
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
      if(data_t_ptr_aux%x .ne. p_num_of_insertions) then
        print *, 'Head element should be: ', p_num_of_insertions, ' but was', data_t_ptr_aux%x
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


    logical function test_list_random_remove_performance() result(test_result)

      type(list_t), pointer :: ll => null()
      type(list_t), pointer :: list_pointer => null()
      type(list_t), pointer :: list_pointer_before => null()
      type(data_t), target  :: dat_a
      type(data_t), pointer :: data_t_ptr_aux

      integer :: an_integer
      integer, parameter :: p_num_of_insertions = 1000000
      integer, parameter :: p_num_of_random_deletions  = 100

      real, parameter :: p_insert_max_time_allowed = 0.2
      real, parameter :: p_visit_max_time_allowed  = 0.1
      real, parameter :: p_remove_max_time_allowed = 10.0
      
      real :: time_initial, time_final 

      integer :: list_size, random_value_of_remove_node 
      real :: random_real_value_aux
      integer,parameter :: seed = 86456
  
      print*, ">>>>> Running Test List random remove Performance"
      test_result = .true.

      call list_init(ll)
      list_size = p_num_of_insertions

      ! Insert values
      
      do an_integer = 1, p_num_of_insertions
        dat_a%x = an_integer
        data_t_ptr_aux => dat_a
        call list_insert(ll, DATA=transfer(data_t_ptr_aux, list_data))
      enddo

      ! remove random positions
      print *, "removing ", p_num_of_random_deletions, " elements of list of size ", p_num_of_insertions
      call random_init(.true. , .true.)
      call cpu_time(time_initial)
      
      do an_integer = 1, p_num_of_random_deletions
        list_pointer => list_next(ll)
        list_pointer_before => list_pointer
        call random_number(random_real_value_aux)
        random_value_of_remove_node = int(random_real_value_aux * list_size)
        random_value_of_remove_node = max(random_value_of_remove_node, 1)
        ! print *, "trying", random_value_of_remove_node

        do 
          data_t_ptr_aux = transfer(list_get(list_pointer), data_t_ptr_aux)
          if (data_t_ptr_aux%x == random_value_of_remove_node) then
            ! print *, "removing item ", data_t_ptr_aux%x
            call list_remove(list_pointer, list_pointer_before)
            list_size = list_size -1
            continue
          endif
          list_pointer_before => list_pointer
          list_pointer => list_next(list_pointer)
          if (.not. associated(list_pointer)) exit
        enddo
      enddo
      call cpu_time(time_final)


      print *, 'Remove Max time = ', p_remove_max_time_allowed, '; Run time = ', time_final-time_initial
      if (time_final-time_initial > p_remove_max_time_allowed) then
        print *, 'Remove Max time exceeded!!!'
        test_result = .false.
        call list_free(ll)
        return
      endif

      call list_free(ll)
      return

    end function

  
end program Modular_tests