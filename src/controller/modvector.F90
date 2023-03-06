module ModVector

  use moddata
  implicit none

  private

  public :: vector_t
  public :: vector_init
  public :: vector_free
  public :: vector_get_num_elements
  public :: vector_insert
  public :: vector_put
  public :: vector_get
  public :: vector_remove

  

  ! Vector data type
  type :: vector_t
    private
    type(data_t), allocatable, dimension(:) :: vector
    integer :: num_elements
    integer :: curr_index
  end type vector_t

contains


  ! Initialize vector
  subroutine vector_init(self, num_elements_input)
    implicit none 
    type(vector_t), intent(inout) :: self
    integer, intent(in) :: num_elements_input

    self%num_elements = num_elements_input
    allocate(self%vector(num_elements_input))
    self%curr_index = 1
  end subroutine vector_init


  ! Free the entire list and all data, beginning at SELF
  subroutine vector_free(self)
    type(vector_t), intent(inout) :: self
    deallocate(self%vector)
    self%curr_index = 1
  end subroutine vector_free


  function vector_get_num_elements(self) result(num_of_elements)
    type(vector_t), intent(in) :: self
    integer :: num_of_elements
    num_of_elements = self%num_elements
  end function vector_get_num_elements

  ! ToDo ....

  ! Insert a value at end of the vector
  subroutine vector_insert(self, data)
    type(vector_t), intent(inout) :: self
    type(data_t), intent(in) :: data
  end subroutine vector_insert


  ! Store the encoded data the index_data position
  subroutine vector_put(self, data, data_index)
    type(vector_t), intent(inout) :: self
    type(data_t), intent(in) :: data
    integer :: data_index
  end subroutine vector_put


  ! Return the DATA stored in the node SELF
  function vector_get(self) result(data)
    type(vector_t), intent(inout) :: self
    type(data_t) :: data
  end function vector_get


    ! Insert a list node after SELF containing DATA (optional)
  subroutine vector_remove(self, data)
    type(vector_t), intent(inout) :: self
    type(data_t), intent(in) :: data
  end subroutine vector_remove


end module ModVector