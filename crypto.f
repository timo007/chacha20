module crypto
use types, only : i4, i8
implicit none
private
public chacha20_block, quarter_round, inner_block

contains

subroutine quarter_round(state, x, y, z, w)
    !
    ! ChaCha quarter round operation as described in RFC 7539.
    !
    ! Arguments:    state       The state vector. Comprised of sixteen 32-bit unsigned integers.
    !                           Because Fortran uses signed integers, we need to store these in
    !                           64-bit integer types.
    !               x, y, z, w  The positions of the four integers in the state vector which will
    !                           be operated on in this quarter round.
    !
    integer(i8), dimension(16)  :: state
    integer                     :: x, y, z, w
    
    state(x) = iand(state(x) + state(y), 4294967295_i8)
    state(w) = ieor(state(w), state(x))
    state(w) = ishftc(state(w), 16, 32)

    state(z) = iand(state(z) + state(w), 4294967295_i8)
    state(y) = ieor(state(y), state(z))
    state(y) = ishftc(state(y), 12, 32)
    
    state(x) = iand(state(x) + state(y), 4294967295_i8)
    state(w) = ieor(state(w), state(x))
    state(w) = ishftc(state(w), 8, 32)
    
    state(z) = iand(state(z) + state(w), 4294967295_i8)
    state(y) = ieor(state(y), state(z))
    state(y) = ishftc(state(y), 7, 32)
    
end subroutine

subroutine inner_block(state)
    !
    ! Two full chacha20 rounds comprising the "inner block", as described in RFC 7539
    !
    ! Argument:     state       The state vector. Comprised of sixteen 32-bit unsigned integers.
    !                           Because Fortran uses signed integers, we need to store these in
    !                           64-bit integer types.
    !
    integer(i8), dimension(16)  :: state
    
    call quarter_round(state, 1, 5, 9, 13)
    call quarter_round(state, 2, 6, 10, 14)
    call quarter_round(state, 3, 7, 11, 15)
    call quarter_round(state, 4, 8, 12, 16)
    call quarter_round(state, 1, 6, 11, 16)
    call quarter_round(state, 2, 7, 12, 13)
    call quarter_round(state, 3, 8, 9, 14)
    call quarter_round(state, 4, 5, 10, 15)

end subroutine

subroutine chacha20_block(key, counter, nonce, block)
    integer(i8), dimension(8)   :: key
    integer(i8)                 :: counter
    integer(i8), dimension(3)   :: nonce
    integer(i8), dimension(16)  :: block
    
    integer(i8), dimension(16)  :: state, working_state
    integer                     :: i
    
    state(1)     = z'61707865'
    state(2)     = z'3320646e'
    state(3)     = z'79622d32'
    state(4)     = z'6b206574'
    state(5:12)  = key(1:8)
    state(13)    = counter
    state(14:16) = nonce(1:3)
    
    working_state = state
    do i = 1, 10
        call inner_block(working_state)
    end do
    
    block = iand(state + working_state, 4294967295_i8)
    
end subroutine

end module
