module Modlist

  use moddata
  implicit none

  private

  public :: list_t
  ! public :: list_data
  public :: init
  public :: free_memory
  public :: insert
  public :: put
  public :: get
  public :: next
  public :: remove

  ! A public variable to use as a MOLD for transfer()
  ! type(data_t), dimension(:), allocatable :: list_data

  ! Linked list node data typetype(data_t), dimension(:), intent(in), optional :: data
  type :: list_t
     private
     type(data_t), allocatable :: data
     type(list_t), pointer :: next => null()
  end type list_t

contains


  ! Initialize a head node SELF and optionally store the provided DATA.
  subroutine init(self, data)
    type(list_t), intent(inout), pointer :: self
    type(data_t), intent(in) :: data

    allocate(self)
    nullify(self%next)

    allocate(self%data)
    self%data = data

  end subroutine init


  ! Free the entire list and all data, beginning at SELF
  subroutine free_memory(self)
    type(list_t), pointer :: self
    type(list_t), pointer :: current
    type(list_t), pointer :: next

    current => self
    do while (associated(current))
       next => current%next
       if (allocated(current%data)) then
          deallocate(current%data)
       end if
       deallocate(current)
       nullify(current)
       current => next
    end do
  end subroutine free_memory


  ! Return the next node after SELF
  function next(self) result(next_returned)
    type(list_t), pointer :: self
    type(list_t), pointer :: next_returned
    next_returned => self%next
  end function next


  ! Insert a list node after SELF containing DATA (optional)
  subroutine insert(self, data)
    type(list_t), intent(inout), pointer :: self
    type(data_t), intent(in), optional :: data
    type(list_t), pointer :: new

    allocate(new)

    if (present(data)) then
       allocate(new%data)
       new%data = data
    end if

    new%next => self
    self => new
  end subroutine insert


  ! Store the encoded DATA in list node SELF
  subroutine put(self, data)
    type(list_t), pointer :: self
    type(data_t), allocatable, intent(in) :: data

    if (allocated(self%data)) then
       deallocate(self%data)
    end if
    allocate(self%data)
    self%data = data
  end subroutine put


  ! Return the DATA stored in the node SELF
  function get(self) result(data)
    type(list_t), pointer :: self
    type(data_t) :: data
    data = self%data
  end function get


    ! Insert a list node after SELF containing DATA (optional)
  subroutine remove(self, before)
    type(list_t), pointer :: self
    type(list_t), pointer :: before

    deallocate(self%data)
    before%next => self%next

  end subroutine remove

end module Modlist