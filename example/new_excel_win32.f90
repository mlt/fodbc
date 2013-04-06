module class_ExcelTest
  use fodbc
  implicit none

  type :: ExcelTest
     type(c_ptr) :: env, dbc, stmt
   contains
     procedure :: run
     ! final :: delete_ExcelTest
  end type ExcelTest

  interface ExcelTest
     module procedure new_ExcelTest
  end interface ExcelTest

contains

 function new_ExcelTest(fname) result(self)
    type(ExcelTest) :: self
    character(len=*), intent(in) :: fname
    integer(C_SHORT) :: err
    integer(C_INT),target :: native
    integer(C_SHORT) :: lens
    character(256, c_char), target :: text
    character(6, c_char), target :: state
    self = ExcelTest(C_NULL_PTR, C_NULL_PTR, C_NULL_PTR)
    print *, "ExcelTest constructor has been called"
    err = SQLAllocHandle(SQL_HANDLE_ENV, C_NULL_PTR, self%env)
    if (err .ne. 0) print *, "Can't allocate environment: ", err
    err = SQLSetEnvAttr(self%env, SQL_ATTR_ODBC_VERSION, transfer(SQL_OV_ODBC3, C_NULL_PTR), 0)
    if (err .ne. 0) print *, "Can't request version", err
    err = SQLAllocHandle(SQL_HANDLE_DBC, self%env, self%dbc)
    if (err .ne. 0) print *, "Can't allocate DBC handle", err
    write (text, '("Driver=Microsoft Excel Driver (*.xls);ReadOnly=False;DBQ=", A)'), fname
    print *, "Connection string: ", text(:len_trim(text))
    err = SQLDriverConnect(self%dbc, C_NULL_PTR, text &
         , int(len_trim(text), c_short), C_STR_NULL_PTR, 0_2, C_SHORT_NULL_PTR, SQL_DRIVER_COMPLETE)
    if (err .ne. 0) print *, "Can't connect", err
    err = 1
    do while (SQL_SUCCESS == SQLGetDiagRec(SQL_HANDLE_DBC, self%dbc, err, state, native, text, int2(sizeof(text)), lens))
       print *, text(1:lens)
       print *, state
       err = err+1
    end do
  end function new_ExcelTest

  subroutine run(self)
    class(ExcelTest) :: self
    type point
       real(c_float) :: x
       real(c_float) :: y
    end type point
    type(point), dimension(100) :: pts
    real :: a
    integer :: i
    integer(C_INT),target :: native
    integer(C_SHORT) :: err, len
    character(256, c_char), target :: text
    character(6, c_char), target :: state

    print *, "Running"
    err = SQLAllocHandle(SQL_HANDLE_STMT, self%dbc, self%stmt);
    if (err .ne. 0) print *, "Can't allocate statement handle", err

    if (SQL_SUCCESS /= SQLExecDirect(self%stmt, C_CHAR_"create table sin (x numeric,y numeric)" // C_NULL_CHAR, SQL_NTS)) &
         print *, "Failed to create a table. Perhaps it already exists."

    a = .1
    pts%x = (/((i*a),i=1,100)/)
    pts%y = sin(pts%x)

    if (SQL_SUCCESS /= SQLPrepare(self%stmt, &
         C_CHAR_"insert into sin values(?,?)" // C_NULL_CHAR, SQL_NTS)) then
       print *, "Failed to prepare statement", err
       return
    end if

    do i = 1,100
       err = SQLBindParameter(self%stmt, 1_2, SQL_PARAM_INPUT, SQL_REAL, &
            0, 0_2, pts(i)%x)
       if (err .ne. 0) print *, "Failed to bind parameter", err

       err = SQLBindParameter(self%stmt, 2_2, SQL_PARAM_INPUT, SQL_REAL, &
            0, 0_2, pts(i)%y)
       if (err .ne. 0) print *, "Failed to bind parameter", err

       err = SQLExecute(self%stmt)
       if (err .ne. 0) print *, "Failed to execute statement", err
       if (SQL_SUCCESS == SQLGetDiagRec(SQL_HANDLE_STMT, self%stmt, 1_2, state, native, text, int(c_sizeof(text), 2), len)) &
            print *, text(1:len)
    end do
  end subroutine run

  subroutine delete_ExcelTest(self)
    use fodbc_types
    class(ExcelTest),intent(in) :: self
    integer(C_SHORT) :: err

    if (c_associated(self%stmt)) then
       if (SQL_SUCCESS .ne. SQLFreeHandle(SQL_HANDLE_STMT, self%stmt)) &
            print *, "Failed to free statement handle", err
    end if
    if (c_associated(self%dbc)) then
       err = SQLDisconnect(self%dbc)
       if (err .ne. 0) print *, "Failed to disconnect", err
       err = SQLFreeHandle(SQL_HANDLE_DBC, self%dbc)
       if (err .ne. 0) print *, "Failed to free DB handle", err
    end if
    if (c_associated(self%env)) then
       err = SQLFreeHandle(SQL_HANDLE_ENV, self%env)
       if (err .ne. 0) print *, "Failed to free environment handle", err
    end if
    print *, "ExcelTest destructor has been called"
  end subroutine delete_ExcelTest
end module class_ExcelTest

program test
  use class_ExcelTest
  use M_kracken
  implicit none
  type(ExcelTest) :: t
  character(len=255) :: filename
  integer :: iflen, ier

  call kracken('cmd','-f hello_fortran.xls')
  call retrev('cmd_f', filename, iflen, ier)

  t = new_ExcelTest(filename(1:iflen))
  call t%run
  call delete_ExcelTest(t)

end program
