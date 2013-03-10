module class_SQLiteTest
  use fodbc_ext
  implicit none

  type :: SQLiteTest
     type(c_ptr) :: env, dbc, stmt
   contains
     procedure :: run
     ! final :: delete_SQLiteTest
  end type SQLiteTest

  interface SQLiteTest
     module procedure new_SQLiteTest
  end interface SQLiteTest

contains

 function new_SQLiteTest() result(self)
    type(SQLiteTest) :: self
    integer(C_SHORT) :: err
    self = SQLiteTest(C_NULL_PTR, C_NULL_PTR, C_NULL_PTR)
    print *, "SQLiteTest constructor has been called"
    err = SQLAllocHandle(SQL_HANDLE_ENV, C_NULL_PTR, self%env)
    if (err .ne. 0) print *, "Can't allocate environment: ", err
    err = SQLSetEnvAttr(self%env, SQL_ATTR_ODBC_VERSION, transfer(SQL_OV_ODBC3, C_NULL_PTR), 0)
    if (err .ne. 0) print *, "Can't request version", err
    err = SQLAllocHandle(SQL_HANDLE_DBC, self%env, self%dbc)
    if (err .ne. 0) print *, "Can't allocate DBC handle", err
    err = SQLDriverConnect(self%dbc, C_NULL_PTR, C_CHAR_"Driver=SQLITE3;Database=/home/mlt/schwinn810.db" // C_NULL_CHAR, &
         SQL_NTS, C_STR_NULL_PTR, 0_2, C_SHORT_NULL_PTR, SQL_DRIVER_COMPLETE)
    if (err .ne. 0) print *, "Can't connect", err
  end function new_SQLiteTest

  subroutine run(self)
    class(SQLiteTest) :: self
    integer(C_SHORT) :: columns, len
    integer(C_INT),target :: speed_max, nrow, native
    type(SQL_TIMESTAMP_STRUCT), target :: start
    character(c_char), target :: track(256)
    integer(C_SHORT) :: err
    integer(C_LONG) :: indicator(3)
    character(c_char), target :: state(6), text(256)

    print *, "Running"
    err = SQLAllocHandle(SQL_HANDLE_STMT, self%dbc, self%stmt);
    if (err .ne. 0) print *, "Can't allocate statement handle", err

    err = SQLExecDirect(self%stmt, C_CHAR_"select count(1) from tracks" // C_NULL_CHAR, -3_4)
    err = SQLFetch(self%stmt)
    err = SQLGetData(self%stmt, 1_2, SQL_INTEGER, c_loc(nrow), sizeof(nrow), indicator(1))
    print *, "We got", nrow, "rows"

    if (SQL_SUCCESS /= SQLCloseCursor(self%stmt)) &
         print *, "Failed to close cursor"

    err = SQLPrepare(self%stmt, C_CHAR_"select speed_max, start, track from tracks" // C_NULL_CHAR, -3_4)
    if (err .ne. 0) print *, "Failed to prepare statement", err

    ! err = SQLGetDiagRec(SQL_HANDLE_STMT, self%stmt, 1_2, state, native, text, int2(sizeof(text)), len)
    ! print *, text(1:len)

    err = SQLBindCol(self%stmt, 1_2, speed_max, indicator(1) )
    if (err .ne. 0) print *, "Failed to bind column", err

    err = SQLBindCol(self%stmt, 2_2, start, indicator(2) )
    if (err .ne. 0) print *, "Failed to bind column", err

    err = SQLBindCol0(self%stmt, 3_2, SQL_CHAR, c_loc(track), sizeof(track), indicator(3) )
    if (err .ne. 0) print *, "Failed to bind column", err

    err = SQLExecute(self%stmt)
    if (err .ne. 0) print *, "Failed to execute statement", err

    err = SQLNumResultCols(self%stmt, columns)
    if (err .ne. 0) print *, "Can't request # of columns", err
    print *, "We got ", columns, " columns"

    print *, "  speed_max   start                                               track"
    err = SQLFetch(self%stmt)
    do while (err .eq. 0)
       if (err .ne. 0) print *, "Failed to fetch", err
       print *, speed_max, start, track(1:indicator(3))
       ! print *, indicator
       err = SQLFetch(self%stmt)
    end do
  end subroutine run

  subroutine delete_SQLiteTest(self)
    use fodbc_types
    class(SQLiteTest),intent(in) :: self
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
    print *, "SQLiteTest destructor has been called"
  end subroutine delete_SQLiteTest
end module class_SQLiteTest

program test
  use class_SQLiteTest
  implicit none
  type(SQLiteTest) :: t

  t = SQLiteTest()
  call t%run
  call delete_SQLiteTest(t)

end program
