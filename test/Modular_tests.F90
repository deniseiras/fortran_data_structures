! A simple generic linked list test program
program Modular_tests

  use Moddata

  implicit none

  logical all_tests_passed 

  print*, ">>>>> Running Modular Tests"
  
  all_tests_passed = .true.
  all_tests_passed = all_tests_passed .and. test_insert_performance()
  all_tests_passed = all_tests_passed .and. test_list_random_remove_performance()

  if (all_tests_passed) then
    print*, ">>>>> All tests OK !"
    call exit(0)
  else
    print*, ">>>>> Some Tests failed!"
    call exit(-1)
  endif


  contains 


    logical function test_insert_performance() result(test_result)
      use Modlist
      implicit none

      type(list_t), pointer :: ll => null()
      type(list_t), pointer :: list_pointer => null()
      type(data_t), allocatable :: dat_a

      integer :: an_integer
      integer, parameter :: p_num_of_insertions = 1000000
      real, parameter :: p_insert_max_time_allowed = 0.2
      real, parameter :: p_visit_max_time_allowed = 0.1
      real time_initial, time_final 


      print*, ">>>>> Running Test List Insert and Visit Performance"
      test_result = .true.

      ! List Insert
      !
      call cpu_time(time_initial)
      allocate(dat_a)
      dat_a%x = 1
      call init(ll, dat_a)
      deallocate(dat_a)
      do an_integer = 2, p_num_of_insertions
        allocate(dat_a)
        dat_a%x = an_integer
        call insert(ll, DATA=dat_a)
        deallocate(dat_a)
      enddo
      call cpu_time(time_final)
      
      ! check performance
      print *, 'Insert Max time = ', p_insert_max_time_allowed, '; Run time = ', time_final-time_initial
      if (time_final-time_initial > p_insert_max_time_allowed) then
        print *, 'Insert Max time exceeded!!!'
        test_result = .false.
        call free_memory(ll)
        return
      endif

      ! check HEAD
      dat_a = get(ll)
      print*, 'Checking head element ...', dat_a%x
      if(dat_a%x /= p_num_of_insertions) then
        print *, 'Head element should be: ', p_num_of_insertions, ' but was', dat_a%x
        test_result = .false.
        call free_memory(ll)
        return
      endif

      ! List Visit test
      call cpu_time(time_initial)
      list_pointer => ll
      do 
        ! do a quick visit
        dat_a = get(list_pointer)
        list_pointer => next(list_pointer)
        if (.not. associated(list_pointer)) exit
      enddo
      call cpu_time(time_final)


      print*, 'Checking last element ...', dat_a%x
      if (dat_a%x /= 1) then
        print *, 'Last element should be 1 but was', dat_a%x
        test_result = .false.
        call free_memory(ll)
        return
      endif


      print *, 'Visit Max time = ', p_visit_max_time_allowed, '; Run time = ', time_final-time_initial
      if (time_final-time_initial > p_visit_max_time_allowed) then
        print *, 'Visit Max time exceeded!!!'
        test_result = .false.
        call free_memory(ll)
        return
      endif

      call free_memory(ll)
      return

    end function


    logical function test_list_random_remove_performance() result(test_result)
      use Modlist
      implicit none

      type(list_t), pointer :: ll => null()
      type(list_t), pointer :: list_pointer => null()
      type(list_t), pointer :: list_pointer_before => null()
      type(data_t), allocatable :: dat_a


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

      list_size = p_num_of_insertions

      ! Insert values
      allocate(dat_a)
      dat_a%x = 1
      call init(ll, dat_a)
      deallocate(dat_a)
      do an_integer = 2, p_num_of_insertions
        allocate(dat_a)
        dat_a%x = an_integer
        call insert(ll, DATA=dat_a)
        deallocate(dat_a)
      enddo

      ! remove random positions
      print *, "removing ", p_num_of_random_deletions, " elements of list of size ", p_num_of_insertions
      call random_init(.true. , .true.)
      call cpu_time(time_initial)
      
      do an_integer = 1, p_num_of_random_deletions
        list_pointer => next(ll)
        list_pointer_before => list_pointer
        call random_number(random_real_value_aux)
        random_value_of_remove_node = int(random_real_value_aux * list_size)
        random_value_of_remove_node = max(random_value_of_remove_node, 1)
        ! print *, "trying", random_value_of_remove_node

        do 
          dat_a = get(list_pointer)
          if (dat_a%x == random_value_of_remove_node) then
            ! print *, "removing item ", data_t_ptr_aux%x
            call remove(list_pointer, list_pointer_before)
            list_size = list_size -1
            exit
          endif
          list_pointer_before => list_pointer
          list_pointer => next(list_pointer)
          if (.not. associated(list_pointer)) exit
        enddo
      enddo
      call cpu_time(time_final)


      print *, 'Remove Max time = ', p_remove_max_time_allowed, '; Run time = ', time_final-time_initial
      if (time_final-time_initial > p_remove_max_time_allowed) then
        print *, 'Remove Max time exceeded!!!'
        test_result = .false.
        call free_memory(ll)
        return
      endif

      call free_memory(ll)
      return

    end function


! TODO
    logical function test_vector_insert_performance() result(test_result)
      use ModVector
      implicit none

    end function

    logical function test_vector_random_remove_performance() result(test_result)
      use ModVector
      implicit none

    end function
  
end program Modular_tests