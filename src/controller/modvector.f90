module ModVector

  use moddata
  implicit none

  private

  public :: vector_t
  public :: vector_data
  public :: vector_init
  public :: vector_free
  public :: vector_insert
  public :: vector_put
  public :: vector_get
  public :: vector_next
  public :: vector_remove


  ! Vector data type
  private
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
    integer, num_elements_input

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



  ! ToDo ....

  ! Insert a list node after SELF containing DATA (optional)

  subroutine vector_insert(self, data)
  end subroutine vector_insert


  ! Store the encoded DATA in list node SELF
  subroutine vector_put(self, data)
  end subroutine vector_put

  ! Return the DATA stored in the node SELF
  function vector_get(self) result(data)
  end function vector_get

    ! Insert a list node after SELF containing DATA (optional)
  subroutine vector_remove(self, before)
  end subroutine vector_remove

end module ModVector