    .text
    .globl main

main:
    move $t0, $sp
    addi $sp, $sp, -72
    sw $fp, 0($sp)
    sw $ra, 4($sp)
    addi $fp, $t0, 0
    addi $t0, $gp, 24
    sw $t0, -4($fp)
    lw $t0, -4($fp)
    sw $t0, -4($sp)
    jal initialize
    sw $v0, -8($fp)
    addi $t0, $gp, 24
    sw $t0, -12($fp)
    lw $t0, -12($fp)
    sw $t0, -4($sp)
    jal sort
    sw $v0, -16($fp)
    li $t0, 0
    sw $t0, -20($fp)
    lw $t0, -20($fp)
    sw $t0, -36($fp)
    li $t0, 8
    sw $t0, -24($fp)
    lw $t1, -36($fp)
    lw $t2, -24($fp)
    slt $t0, $t1, $t2
    sw $t0, -28($fp)
label0:
    lw $t0, -28($fp)
    beq $t0, $zero, label1
    addi $t0, $gp, 24
    sw $t0, -60($fp)
    li $t0, 4
    sw $t0, -40($fp)
    lw $t1, -40($fp)
    lw $t2, -36($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -44($fp)
    lw $t1, -60($fp)
    lw $t2, -44($fp)
    add $t0, $t1, $t2
    sw $t0, -64($fp)
    lw $t1, -64($fp)
    lw $t0, 0($t1)
    sw $t0, -56($fp)
    li $v0, 1
    lw $a0, -56($fp)
    syscall 
    li $t0, 1
    sw $t0, -48($fp)
    lw $t1, -36($fp)
    lw $t2, -48($fp)
    add $t0, $t1, $t2
    sw $t0, -52($fp)
    lw $t0, -52($fp)
    sw $t0, -36($fp)
    li $t0, 8
    sw $t0, -24($fp)
    lw $t1, -36($fp)
    lw $t2, -24($fp)
    slt $t0, $t1, $t2
    sw $t0, -28($fp)
    j label0
label1:
    li $t0, 0
    sw $t0, -56($fp)
    lw $v0, -56($fp)
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 72
    jr $ra
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 72
    jr $ra
initialize:
    move $t0, $sp
    addi $sp, $sp, -52
    sw $fp, 0($sp)
    sw $ra, 4($sp)
    addi $fp, $t0, -4
    li $t0, 0
    sw $t0, -4($fp)
    lw $t0, -4($fp)
    sw $t0, -16($fp)
    li $t0, 8
    sw $t0, -8($fp)
    lw $t1, -16($fp)
    lw $t2, -8($fp)
    slt $t0, $t1, $t2
    sw $t0, -12($fp)
label2:
    lw $t0, -12($fp)
    beq $t0, $zero, label3
    li $t0, 4
    sw $t0, -24($fp)
    lw $t1, -24($fp)
    lw $t2, -16($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -28($fp)
    lw $t1, 0($fp)
    lw $t2, -28($fp)
    add $t0, $t1, $t2
    sw $t0, -20($fp)
    lw $t0, -16($fp)
    sw $t0, -4($sp)
    jal num
    sw $v0, -32($fp)
    lw $t1, -32($fp)
    lw $t0, -20($fp)
    sw $t1, 0($t0)
    li $t0, 1
    sw $t0, -36($fp)
    lw $t1, -16($fp)
    lw $t2, -36($fp)
    add $t0, $t1, $t2
    sw $t0, -40($fp)
    lw $t0, -40($fp)
    sw $t0, -16($fp)
    li $t0, 8
    sw $t0, -8($fp)
    lw $t1, -16($fp)
    lw $t2, -8($fp)
    slt $t0, $t1, $t2
    sw $t0, -12($fp)
    j label2
label3:
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 52
    jr $ra
sort:
    move $t0, $sp
    addi $sp, $sp, -160
    sw $fp, 0($sp)
    sw $ra, 4($sp)
    addi $fp, $t0, -4
    li $t0, 0
    sw $t0, -4($fp)
    lw $t0, -4($fp)
    sw $t0, -16($fp)
    li $t0, 7
    sw $t0, -8($fp)
    lw $t1, -16($fp)
    lw $t2, -8($fp)
    slt $t0, $t1, $t2
    sw $t0, -12($fp)
label4:
    lw $t0, -12($fp)
    beq $t0, $zero, label5
    li $t0, 1
    sw $t0, -28($fp)
    lw $t1, -16($fp)
    lw $t2, -28($fp)
    add $t0, $t1, $t2
    sw $t0, -32($fp)
    lw $t0, -32($fp)
    sw $t0, -20($fp)
    li $t0, 8
    sw $t0, -36($fp)
    lw $t1, -20($fp)
    lw $t2, -36($fp)
    slt $t0, $t1, $t2
    sw $t0, -40($fp)
label6:
    lw $t0, -40($fp)
    beq $t0, $zero, label7
    li $t0, 4
    sw $t0, -92($fp)
    lw $t1, -92($fp)
    lw $t2, -16($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -52($fp)
    lw $t1, 0($fp)
    lw $t2, -52($fp)
    add $t0, $t1, $t2
    sw $t0, -88($fp)
    lw $t1, -88($fp)
    lw $t0, 0($t1)
    sw $t0, -84($fp)
    li $t0, 4
    sw $t0, -64($fp)
    lw $t1, -64($fp)
    lw $t2, -20($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -68($fp)
    lw $t1, 0($fp)
    lw $t2, -68($fp)
    add $t0, $t1, $t2
    sw $t0, -60($fp)
    lw $t1, -60($fp)
    lw $t0, 0($t1)
    sw $t0, -56($fp)
    lw $t1, -84($fp)
    lw $t2, -56($fp)
    slt $t0, $t2, $t1
    sw $t0, -72($fp)
    lw $t0, -72($fp)
    beq $t0, $zero, label8
    li $t0, 4
    sw $t0, -104($fp)
    lw $t1, -104($fp)
    lw $t2, -16($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -108($fp)
    lw $t1, 0($fp)
    lw $t2, -108($fp)
    add $t0, $t1, $t2
    sw $t0, -100($fp)
    lw $t1, -100($fp)
    lw $t0, 0($t1)
    sw $t0, -96($fp)
    lw $t0, -96($fp)
    sw $t0, -24($fp)
    li $t0, 4
    sw $t0, -116($fp)
    lw $t1, -116($fp)
    lw $t2, -16($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -120($fp)
    lw $t1, 0($fp)
    lw $t2, -120($fp)
    add $t0, $t1, $t2
    sw $t0, -112($fp)
    li $t0, 4
    sw $t0, -132($fp)
    lw $t1, -132($fp)
    lw $t2, -20($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -136($fp)
    lw $t1, 0($fp)
    lw $t2, -136($fp)
    add $t0, $t1, $t2
    sw $t0, -128($fp)
    lw $t1, -128($fp)
    lw $t0, 0($t1)
    sw $t0, -124($fp)
    lw $t1, -124($fp)
    lw $t0, -112($fp)
    sw $t1, 0($t0)
    li $t0, 4
    sw $t0, -144($fp)
    lw $t1, -144($fp)
    lw $t2, -20($fp)
    mult $t1, $t2
    mflo $t0
    sw $t0, -148($fp)
    lw $t1, 0($fp)
    lw $t2, -148($fp)
    add $t0, $t1, $t2
    sw $t0, -140($fp)
    lw $t1, -24($fp)
    lw $t0, -140($fp)
    sw $t1, 0($t0)
    j label9
label8:

label9:
    li $t0, 1
    sw $t0, -96($fp)
    lw $t1, -20($fp)
    lw $t2, -96($fp)
    add $t0, $t1, $t2
    sw $t0, -100($fp)
    lw $t0, -100($fp)
    sw $t0, -20($fp)
    li $t0, 8
    sw $t0, -36($fp)
    lw $t1, -20($fp)
    lw $t2, -36($fp)
    slt $t0, $t1, $t2
    sw $t0, -40($fp)
    j label6
label7:
    li $t0, 1
    sw $t0, -84($fp)
    lw $t1, -16($fp)
    lw $t2, -84($fp)
    add $t0, $t1, $t2
    sw $t0, -88($fp)
    lw $t0, -88($fp)
    sw $t0, -16($fp)
    li $t0, 7
    sw $t0, -8($fp)
    lw $t1, -16($fp)
    lw $t2, -8($fp)
    slt $t0, $t1, $t2
    sw $t0, -12($fp)
    j label4
label5:
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 160
    jr $ra
num:
    move $t0, $sp
    addi $sp, $sp, -40
    sw $fp, 0($sp)
    sw $ra, 4($sp)
    addi $fp, $t0, -4
    li $t0, 2
    sw $t0, -4($fp)
    lw $t1, 0($fp)
    lw $t2, -4($fp)
    beq $t1, $t2, label10
    li $t0, 0
    j label11
label10:
    li $t0, 1
label11:
    sw $t0, -8($fp)
    li $t0, 4
    sw $t0, -12($fp)
    lw $t1, 0($fp)
    lw $t2, -12($fp)
    beq $t1, $t2, label12
    li $t0, 0
    j label13
label12:
    li $t0, 1
label13:
    sw $t0, -16($fp)
    lw $t0, -8($fp)
    beq $t0, $zero, label14
    li $t0, 1
    sw $t0, -20($fp)
    j label15
label14:
    lw $t0, -16($fp)
    beq $t0, $zero, label16
    li $t0, 1
    sw $t0, -20($fp)
    j label17
label16:
    li $t0, 0
    sw $t0, -20($fp)
label17:
label15:
    lw $t0, -20($fp)
    beq $t0, $zero, label18
    lw $v0, 0($fp)
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 40
    jr $ra
    j label19
label18:
    li $t0, 8
    sw $t0, -24($fp)
    lw $t1, -24($fp)
    lw $t2, 0($fp)
    sub $t0, $t1, $t2
    sw $t0, -28($fp)
    lw $v0, -28($fp)
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 40
    jr $ra
label19:
    lw $ra, 4($sp)
    lw $fp, 0($sp)
    addi $sp, $sp, 40
    jr $ra
