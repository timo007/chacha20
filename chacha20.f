program chacha20
use types, only:    i8
use crypto, only:   chacha20_block
implicit none

integer(kind=i8), dimension(16) :: block = 0_i8
integer(kind=i8), dimension(8)  :: key
integer(kind=i8), dimension(3)  :: nonce
integer(kind=i8)                :: counter

key(1) = z'03020100'
key(2) = z'07060504'
key(3) = z'0b0a0908'
key(4) = z'0f0e0d0c'
key(5) = z'13121110'
key(6) = z'17161514'
key(7) = z'1b1a1918'
key(8) = z'1f1e1d1c'

nonce(1) = z'09000000'
nonce(2) = z'4a000000'
nonce(3) = z'00000000'

counter = 1_i8

call chacha20_block(key, counter, nonce, block)

write (*, '(4(4Z9.8/))') block(1:16)

end program chacha20
