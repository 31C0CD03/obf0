// Source
#include <stdio.h>

[[clang::optnone]]
[[clang::noinline]]
int foo(int x) {
    return x + 1 - 2;
}

int main() {
    int x;
    scanf("%d", &x);
    for (int i = 0; i < x; i++) printf("\t%d\n", foo(i + 2));
    return foo(1);
}

// Unobfuscated
int _foo(int param_1)
{
    return param_1 + -1;
}

void entry(void)
{
    int iVar1;
    int local_24;
    
    _scanf("%d");
    if (0 < local_24) {
        iVar1 = 0;
        do {
            _foo(iVar1 + 2);
            _printf("\t%d\n");
            iVar1 = iVar1 + 1;
        } while (iVar1 < local_24);
    }
    _foo(1);
    return;
}

// Obfuscated / PPC { MBA(1) . OP(1) }
uint _foo(uint param_1)
{
    return (((param_1 ^ 0xffffffff) & 1) + (param_1 & 0xfffffffe) + (param_1 & 1) * 2 ^ 0xffffffff) +
                 2 ^ 0xffffffff;
}

void entry(void)
{
    byte *pbVar1;
    uint uVar2;
    uint uVar3;
    uint uVar4;
    undefined8 uVar5;
    undefined1 *puVar6;
    undefined1 *puVar7;
    uint uVar8;
    undefined1 auStack_60 [12];
    int local_54;
    
    _scanf("%d");
    puVar7 = auStack_60;
    if (0 < local_54) {
        uVar8 = 0;
        do {
            while( true ) {
                puVar6 = puVar7 + -0x10;
                puVar7[-0x10] = 1;
                if (puVar7[-0x10] != '\x01') break;
LAB_100000504:
                uVar5 = _foo((uVar8 & 0xfffffffc | uVar8 & 1 | (~uVar8 >> 1 & 1) << 1) + (uVar8 & 2) * 2);
                *(undefined8 *)(puVar6 + -0x10) = uVar5;
                _printf("\t%d\n");
                uVar8 = (uVar8 & 0xfffffffe | ~uVar8 & 1) + (uVar8 & 1) * 2;
                puVar7 = puVar6 + -0x10;
                puVar6[-0x10] = 1;
                if (local_54 <= (int)uVar8) goto LAB_100000624;
            }
            uVar5 = _foo((uVar8 & 0xfffffffc | uVar8 & 1 | (~uVar8 >> 1 & 1) << 1) + (uVar8 & 2) * 2);
            *(undefined8 *)(puVar7 + -0x20) = uVar5;
            _printf("\t%d\n");
            uVar2 = (uVar8 & 0xfffffffe | ~uVar8 & 1) + (uVar8 & 1) * 2;
            *(undefined4 *)(puVar7 + -0x20) = 0xcd9;
            uVar3 = *(uint *)(puVar7 + -0x20);
            *(undefined4 *)(puVar7 + -0x30) = 0x65e;
            uVar4 = *(uint *)(puVar7 + -0x30);
            _DAT_00001337 =
                     (uVar4 & (uVar3 ^ 0xffffffff) | uVar3 & (uVar4 ^ 0xffffffff)) + (uVar3 & uVar4) * 2;
            puVar6 = puVar7 + -0x40;
            puVar7[-0x40] = 1;
            if ((puVar7[-0x40] & 1) == 0) goto LAB_100000504;
            puVar7 = puVar6;
            uVar8 = uVar2;
        } while ((int)uVar2 < local_54);
    }
LAB_100000624:
    puVar6 = puVar7 + -0x10;
    puVar7[-0x10] = 1;
    if ((puVar7[-0x10] & 1) == 0) {
        do {
            _foo(1);
            *(undefined4 *)(puVar6 + -0x10) = 0x7ff;
            uVar8 = *(uint *)(puVar6 + -0x10);
            *(undefined4 *)(puVar6 + -0x20) = 0xb38;
            uVar2 = *(uint *)(puVar6 + -0x20);
            _DAT_00001337 =
                     (uVar2 & (uVar8 ^ 0xffffffff) | uVar8 & (uVar2 ^ 0xffffffff)) + (uVar8 & uVar2) * 2;
            puVar6[-0x30] = 1;
            pbVar1 = puVar6 + -0x30;
            puVar6 = puVar6 + -0x30;
        } while ((*pbVar1 & 1) == 0);
    }
    else {
        _foo(1);
        puVar7[-0x20] = 1;
    }
    return;
}
