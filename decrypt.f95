program decrypt
    ! Nicholas Maynard
    ! CSI 501
    ! Homework 4.1
    ! 02/23/23

    ! This program is meant to read in a file that contains an encryption matrix and a message 
    ! and then applies the inversion of the matrix to decrypt the message. The end result should
    ! be a printed out message contained in the file.

    ! Clear memory for a clean start
    implicit none
    
    ! Declare varialble
    integer :: row, detM, determinant, charn
    integer :: n = 2
    integer, allocatable :: EncryptM(:,:), InversionM(:,:), dat(:)
    character(len=32), allocatable :: message
    
    ! Open the input and output files.
    open(13,file='Data3.txt')

    ! Allocate memory for encryption matrix
    allocate(EncryptM(n,n))

    print*, "The Encryption Matrix is: "

    ! Read and display the matrix
    do row = 1, n
        read(13,*) EncryptM(row,:)
        print*, EncryptM(row, :)
    enddo

    print*, "The inversion Matrix is: "

    ! Allocate memory for it
    allocate(InversionM(n,n))
    message = ''
    ! calculate determinant for inverse value.
    detM = determinant(EncryptM, 2)

    ! Do calculations for matrix inversion.
    InversionM(1,1) = EncryptM(2,2) / detM
    InversionM(1,2) = -EncryptM(1,2) / detM
    InversionM(2,1) = -EncryptM(2,1) / detM
    InversionM(2,2) = EncryptM(1,1) / detM

    ! Display inverted matrix
    do row = 1, n
        print*, InversionM(row,:)
    end do

    ! Allocate the array for our data as we read it in 2 at a time.
    allocate(dat(n))
    message = ''

    ! Iterate through our data, decode the character, and store the results.
    do row=1, 32, n
        read(13,*) dat
        charn = (InversionM(1,1) * dat(1)) + (InversionM(1,2) * dat(2))
        message = trim(message) // trim(char(charn))
        charn = (InversionM(2,1) * dat(1)) + (InversionM(2,2) * dat(2))
        message = trim(message) // trim(char(charn)) 
    enddo

    ! print the ~secret~ message.
    print*, message

    ! Clean up the memory
    deallocate(dat)
    deallocate(InversionM)
    deallocate(EncryptM)
 
 end program decrypt

 function determinant(M, n) result(Det)
    ! Clear the memory for the variables
    implicit none
 
    ! Define the variable types
    integer :: Det
    integer :: n
    integer :: M(n,n)
 
    ! Perform determinant for 2x2 matrix.
    if ( n == 2 ) then
       Det = (M(1,1) * M(2,2)) - (M(1,2) * M(2,1))
    
    ! Perform determinant for 3x3 matrix.
    else if ( n == 3 ) then
       Det = (M(1,1) * M(2,2) * M(3,3)) + (M(1,2) * M(2,3) * M(3,1)) + (M(1,3) * M(2,1) * M(3,2)) &
       - (M(1,3) * M(2,2) * M(3,1)) - (M(1,2) * M(2,1) * M(3,3)) - (M(1,1) * M(2,3) * M(3,2))
    end if
 
       
 end function determinant