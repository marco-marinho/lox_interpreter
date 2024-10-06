type expression =
| BinaryExpr of expression * Token.token * expression
| GroupingExpr of expression
| LiteralExpr of Token.literal
| UnaryExpr of Token.token * expression