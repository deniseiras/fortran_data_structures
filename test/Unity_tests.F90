! A simple generic linked list test program
program Unity_tests

  use Moddata

  implicit none

  integer :: test_errors_sum
  
  print*, ""
  print*, ">>>>> Running Unity Tests"

  test_errors_sum = 0
  test_errors_sum = test_errors_sum + test_list_init()
  test_errors_sum = test_errors_sum + test_list_inserts()
  test_errors_sum = test_errors_sum + test_list_removes()
  test_errors_sum = test_errors_sum + test_vector_init()
  test_errors_sum = test_errors_sum + test_vector_inserts()
  ! test_errors_sum = test_errors_sum + test_vector_removes()
  
  print *, "" 
  if (test_errors_sum == 0) then
    print*, ">>>>> ALL TESTS OK !"
    call exit(0)
  else
    print*, ">>>>> TESTS FAILED. TOTAL TESTS FAILED = ", test_errors_sum
    call exit(-1)
  endif


  contains 

    ! List Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    integer function test_list_init() result(test_error)
      use Modlist
      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), allocatable :: dat_a
      type(data_t) :: dat_test

      print*, ""
      print*, ">>>>> Running Test List Initilize"
      test_error = 0

      allocate(dat_a)
      dat_a%x = 1

      call init(ll, dat_a)
      print *, 'Initializing list with data:', dat_a

      print *, 'Testing head node'      
      dat_test = get(ll)
      if (dat_test%x .ne. 1) then
        print *, 'Head node data should be: 1 but was', dat_test%x
        test_error = 1
      endif
      
      call free_memory(ll)
      return

    end function


    integer function test_list_inserts() result(test_error)
      use Modlist
      implicit none 

      type(list_t), pointer :: ll => null()
      type(data_t), allocatable :: dat_x
      type(data_t) :: dat_test

      print*, ""
      print*, ">>>>> Running Test List Insert second element"
      test_error = 0

      allocate(dat_x)
      dat_x%x = 1
      call init(ll, dat_x)
      print *, 'Initializing list with data:', dat_x%x
      deallocate(dat_x)

      allocate(dat_x)
      dat_x%x = 2
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%x
      deallocate(dat_x)

      print *, 'Testing head node'      
      dat_test = get(ll)
      if (dat_test%x .ne. 2) then
        print *, '!!!!! TEST FAILED !!!!! Head node data should be: 2 but was', dat_test%x
        call free_memory(ll)
        test_error = 1
        return
      endif
      
      print *, 'Testing second node'
      dat_test = get(next(ll))
      if (dat_test%x .ne. 1) then
        print *, '!!!!! TEST FAILED !!!!! Second node data should be: 1 but was', dat_test%x
        call free_memory(ll)
        test_error = 1
        return
      endif

      ! Free the list
      call free_memory(ll)

    end function

    integer function test_list_removes() result(test_error)
      use Modlist
      implicit none 

      type(list_t), pointer :: ll, node_curr, node_before => null()
      type(data_t), allocatable :: dat_x
      type(data_t) :: dat_test
      integer :: test_value

      print*, ""
      print*, ">>>>> Running Test List removes elements"
      test_error = 0

      allocate(dat_x)
      dat_x%x = 10
      call init(ll, dat_x)
      print *, 'Initializing list with data:', dat_x%x
      deallocate(dat_x)

      allocate(dat_x)
      dat_x%x = 20
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%x
      deallocate(dat_x)

      allocate(dat_x)
      dat_x%x = 30
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%x
      deallocate(dat_x)

      allocate(dat_x)
      dat_x%x = 40
      call insert(ll, dat_x)
      print *, 'Inserting node with data:', dat_x%x
      deallocate(dat_x)


      print *, 'removes last element'
      node_curr => ll
      do 
        if (.not. associated(next(node_curr))) exit
        node_before => node_curr
        node_curr => next(node_curr)
      enddo
      call remove(node_curr, node_before)

      print *, 'Testing nodes'      
      node_curr => ll
      test_value = 40
      do
        dat_test = get(node_curr)
        print *, 'Checking node ', dat_test%x
        if (dat_test%x .ne. test_value) then
          print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%x
          call free_memory(ll)
          test_error = 1
          return
        endif
        if (.not. associated(next(node_curr))) exit
        node_curr => next(node_curr)
        test_value = test_value - 10
      enddo
      
      print *, 'removes second element'
      node_curr => next(ll)
      call remove(node_curr, ll)
      node_curr => ll
      dat_test = get(node_curr)
      print *, 'Checking node ', dat_test%x
      test_value = 40
      if (dat_test%x .ne. test_value) then
        print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%x
        call free_memory(ll)
        test_error = 1
        return
      endif
      node_curr => next(node_curr)
      dat_test = get(node_curr)
      print *, 'Checking node ', dat_test%x
      test_value = 20
      if (dat_test%x .ne. test_value) then
        print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%x
        call free_memory(ll)
        test_error = 1
        return
      endif

      print *, 'removes first element'
      node_curr => next(ll)
      call remove(ll)
      dat_test = get(node_curr)
      print *, 'Checking node ', dat_test%x
      test_value = 20
      if (dat_test%x .ne. test_value) then
        print *, '!!!!! TEST FAILED !!!!! Node data should be: ',test_value,' but was', dat_test%x
        call free_memory(ll)
        test_error = 1
        return
      endif

    end function

    ! Vector Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    integer function test_vector_init() result(test_error)
      use ModVector
      implicit none 

      type(vector_t) :: vec
      integer, parameter :: p_vector_size = 1000000

      print*, ""
      print*, ">>>>> Running Test Vector Initilize"
      test_error = 0


      print *, 'Testing get_size = 0'
      if(get_size(vec) .ne. 0) then
        print *, '!!!!! TEST FAILED !!!!! Size of vector should be: 0 but was', get_size(vec)
        test_error = 1
        call free_memory(vec)
        return
      endif

      print *, 'Initializing Vector with size = ', p_vector_size
      call init(vec, p_vector_size)

      print *, 'Testing get_size = ', p_vector_size
      if(get_size(vec) .ne. p_vector_size) then
        print *, '!!!!! TEST FAILED !!!!! Size of vector should be: ', p_vector_size,' but was', get_size(vec)
        test_error = 1
        call free_memory(vec)
        return
      endif

      print *, 'Testing num_elements = 0'
      if(get_num_elements(vec) .ne. 0) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: 0 but was', get_num_elements(vec)
        test_error = 1
        call free_memory(vec)
        return
      endif

      call free_memory(vec)

    end function


    integer function test_vector_inserts() result(test_error)
      use ModVector
      implicit none 

      type(vector_t) :: vec
      type(data_t), allocatable :: dat_a
      type(data_t) :: dat_test
      integer, parameter :: p_vector_size = 1000000
      
      print*, ""
      print*, ">>>>> Running Test Vector Insert elements"
      test_error = 0

      print *, 'Initializing Vector with size = ', p_vector_size
      call init(vec, p_vector_size)

      allocate(dat_a)
      dat_a%x = 10
      print *, 'Inserting vector element with: ', dat_a%x
      call insert(vec, dat_a)
      deallocate(dat_a)

      print *, 'Testing num_elements = 1'
      if(get_num_elements(vec) .ne. 1) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: 1 but was', get_num_elements(vec)
        test_error = 1
        call free_memory(vec)
        return
      endif

      print *, 'Testing first element'
      dat_test = get(vec, get_num_elements(vec))
      if(dat_test%x .ne. 10) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: 10 but was', dat_test%x
        test_error = 1
        call free_memory(vec)
        return
      endif

      allocate(dat_a)
      dat_a%x = 20
      print *, 'Inserting vector element with: ', dat_a%x
      call insert(vec, dat_a)
      deallocate(dat_a)

      print *, 'Testing num_elements = 2'
      if(get_num_elements(vec) .ne. 2) then
        print *, '!!!!! TEST FAILED !!!!! Num elements should be: 2 but was', get_num_elements(vec)
        test_error = 1
        call free_memory(vec)
        return
      endif

      print *, 'Testing second element'
      dat_test = get(vec, get_num_elements(vec))
      if(dat_test%x .ne. 20) then
        print *, '!!!!! TEST FAILED !!!!! First element should be: 20 but was', dat_test%x
        test_error = 1
        call free_memory(vec)
        return
      endif


      call free_memory(vec)


    end function

    

end program Unity_tests