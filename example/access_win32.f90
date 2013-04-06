module class_AccessTest
  use fodbc
  implicit none

  type :: AccessTest
     type(c_ptr) :: env, dbc, stmt
   contains
     procedure :: run
     ! final :: delete_AccessTest
  end type AccessTest

  interface AccessTest
     module procedure new_AccessTest
  end interface AccessTest

contains

 function new_AccessTest() result(self)
    type(AccessTest) :: self
    integer(C_SHORT) :: err
    integer(C_INT),target :: native
    integer(C_SHORT) :: len
    character(6, c_char), target :: state
    character(256, c_char), target :: text
    self = AccessTest(C_NULL_PTR, C_NULL_PTR, C_NULL_PTR)
    print *, "AccessTest constructor has been called"
    err = SQLAllocHandle(SQL_HANDLE_ENV, C_NULL_PTR, self%env)
    if (err .ne. 0) print *, "Can't allocate environment: ", err
    err = SQLSetEnvAttr(self%env, SQL_ATTR_ODBC_VERSION, transfer(SQL_OV_ODBC3, C_NULL_PTR), 0)
    if (err .ne. 0) print *, "Can't request version", err
    err = SQLAllocHandle(SQL_HANDLE_DBC, self%env, self%dbc)
    if (err .ne. 0) print *, "Can't allocate DBC handle", err
    err = SQLDriverConnect(self%dbc, C_NULL_PTR, &
         C_CHAR_"Driver=Microsoft Access Driver (*.mdb);DBQ=D:\webdata\soils\soil_mn091\soildb_MN_2003.mdb" // C_NULL_CHAR, &
         SQL_NTS2, C_STR_NULL_PTR, 0_2, C_SHORT_NULL_PTR, SQL_DRIVER_COMPLETE)
    if (err .ne. 0) print *, "Can't connect", err
    if (SQL_SUCCESS == SQLGetDiagRec(SQL_HANDLE_DBC, self%dbc, 1_2, state, native, text, int(c_sizeof(text), 2), len)) &
         print *, text(1:len)
  end function new_AccessTest

  subroutine run(self)
    class(AccessTest) :: self
    integer(C_SHORT) :: columns, len
    integer(C_INT),target :: speed_max, nrow, native
    type(SQL_TIMESTAMP_STRUCT), target :: start
    character(c_char), target :: track(256)
    integer(C_SHORT) :: err
    integer(C_LONG) :: indicator(3), iName
    character(256, c_char), target :: text
    character(6, c_char), target :: state
    character(c_char),target :: name(256)

    print *, "Running"
    err = SQLAllocHandle(SQL_HANDLE_STMT, self%dbc, self%stmt);
    if (err .ne. 0) print *, "Can't allocate statement handle", err

    nrow = 0
    if (SQL_SUCCESS == SQLExecDirect(self%stmt, C_CHAR_"select count(1) from component" // C_NULL_CHAR, SQL_NTS) .and. &
         SQL_SUCCESS == SQLFetch(self%stmt) .and. &
         SQL_SUCCESS == SQLGetData(self%stmt, 1_2, nrow)) &
         print *, "We got", nrow, "rows"

    if (SQL_SUCCESS /= SQLCloseCursor(self%stmt)) &
         print *, "Failed to close cursor"

    err = SQLPrepare(self%stmt, &
         C_CHAR_"select comppct_r, compname, mukey from component where compname=?" // C_NULL_CHAR, SQL_NTS)
    if (err .ne. 0) print *, "Failed to prepare statement", err

    ! WTF? See http://fortranwiki.org/fortran/show/generating+c+interfaces#strings
    do err = 1, 7
       name(err) = "Webster"(err:err)
    end do
    name(8) = C_NULL_CHAR
    err = SQLBindParameter0(self%stmt, 1_2, SQL_PARAM_INPUT, SQL_CHAR, SQL_VARCHAR, &
         255, 0_2, c_loc(name), 0, iName )
    if (err .ne. 0) print *, "Failed to bind parameter", err

    if (SQL_SUCCESS == SQLGetDiagRec(SQL_HANDLE_STMT, self%stmt, 1_2, state, native, text, int(c_sizeof(text), 2), len)) &
         print *, text(1:len)

    err = SQLBindCol(self%stmt, 1_2, speed_max, indicator(1) )
    if (err .ne. 0) print *, "Failed to bind column", err

    err = SQLBindCol(self%stmt, 2_2, SQL_CHAR, c_loc(track), sizeof(track), indicator(3) )
    if (err .ne. 0) print *, "Failed to bind column", err

    err = SQLExecute(self%stmt)
    if (err .ne. 0) print *, "Failed to execute statement", err
    if (SQL_SUCCESS == SQLGetDiagRec(SQL_HANDLE_STMT, self%stmt, 1_2, state, native, text, int2(sizeof(text)), len)) &
         print *, text(1:len)

    err = SQLNumResultCols(self%stmt, columns)
    if (err .ne. 0) print *, "Can't request # of columns", err
    print *, "We got ", columns, " columns"

    print *, "  comppct_t   name"
    err = SQLFetch(self%stmt)
    do while (err .eq. 0)
       if (err .ne. 0) print *, "Failed to fetch", err
       print *, speed_max, track(1:indicator(3))
       ! print *, indicator
       err = SQLFetch(self%stmt)
    end do
  end subroutine run

  subroutine delete_AccessTest(self)
    use fodbc_types
    class(AccessTest),intent(in) :: self
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
    print *, "AccessTest destructor has been called"
  end subroutine delete_AccessTest
end module class_AccessTest

program test
  use class_AccessTest
  implicit none
  type(AccessTest) :: t

  t = AccessTest()
  call t%run
  call delete_AccessTest(t)

end program
