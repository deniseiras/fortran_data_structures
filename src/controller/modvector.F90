module ModVector

  use moddata
  implicit none

  private

  public :: vector_t
  public :: init
  public :: free_memory
  public :: get_size
  public :: get_num_elements
  public :: insert
  public :: vector_put
  public :: get
  public :: remove
  

  ! Vector data type
  type :: vector_t
    private
    type(data_t), allocatable, dimension(:) :: vector
    integer :: num_elements
    
  end type vector_t

contains


  ! Initialize vector
  subroutine init(self, vec_max_size)
    implicit none 
    type(vector_t), intent(inout) :: self
    integer, intent(in) :: vec_max_size

    self%num_elements = 0
    allocate(self%vector(vec_max_size))
  end subroutine init


  ! Free the entire list and all data, beginning at SELF
  subroutine free_memory(self)
    type(vector_t), intent(inout) :: self

    if(allocated(self%vector)) deallocate(self%vector)
    self%num_elements = 0
  end subroutine free_memory


  function get_size(self) result(size_vec)
    type(vector_t), intent(in) :: self
    integer :: size_vec
    if(allocated(self%vector)) then
      size_vec = size(self%vector)
    else
      size_vec = 0
    endif
  end function get_size


  function get_num_elements(self) result(num_elements)
    type(vector_t), intent(in) :: self
    integer :: num_elements
    num_elements = self%num_elements
  end function get_num_elements

  ! ToDo ....

  ! Insert a value at end of the vector
  subroutine insert(self, data)
    type(vector_t), intent(inout) :: self
    type(data_t), intent(in) :: data

    self%num_elements = self%num_elements + 1
    self%vector(self%num_elements) = data
  end subroutine insert


  ! Store the encoded data the index_data position
  subroutine vector_put(self, data, data_index)
    type(vector_t), intent(inout) :: self
    type(data_t), intent(in) :: data
    integer, intent(in) :: data_index

    self%vector(data_index) = data
  end subroutine vector_put


  ! Return the DATA stored in data_index
  function get(self, data_index) result(data)
    type(vector_t), intent(inout) :: self
    integer, intent(in) :: data_index
    type(data_t) :: data
    data = self%vector(data_index)
  end function get


    ! Insert a list node after SELF containing DATA (optional)
  subroutine remove(self, data_index)
    type(vector_t), intent(inout) :: self
    integer, intent(in) :: data_index

    if (self%num_elements == 0) then
      print *, '***** trying to remove index ', data_index,' from an empty vector. Ignoring'
      return
    endif

    self%vector(data_index:size(self%vector)-1) = self%vector(data_index+1:size(self%vector))
    self%num_elements = self%num_elements -1
  end subroutine remove


end module ModVector