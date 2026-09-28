
typedef enum RexAccType {
    REX_ACC_NONE,
    REX_ACC_SKIP,
    REX_ACC,
} RexAccType;

typedef struct RexAcc {
    RexAccType type;
    int state;
    RexInput input;
    union {
        struct {
            RexInput inp0;
            int leng;
        } token;
    };
} RexAcc;
