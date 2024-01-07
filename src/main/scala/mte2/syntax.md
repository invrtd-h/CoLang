## Syntax for CoLang v2

### Concrete Syntax

```text
id         ::= (defined by regex)
num        ::= (defined by regex)
str        ::= "char*"
type-var   ::= 'id
visibility ::= 내수용
             | NULL
program    ::= 춘잣! { expr }
expr       ::= id[type, ...]?
             | num
             | ㅖ
             | 이거나 드셔
             | 스킵이야
             | 스키비야
             | str
             | unary-lop expr
             | expr unary-rop
             | expr binary-op expr
             | 유링게슝한? (expr) expr
             | 유링게슝한? (expr) expr 안유링게슝 expr
             | 인 (expr) 중에는! expr
             | {expr; ...; expr}
             | 묶음!!(expr, ...?)
             | expr 의 id 감동님 사랑해
             | 아니 세상에 자기가 (
                   d: type?
               ) 라는/이라는 사람인데 expr 을/를 했대
             | expr 의 id 감독님/감동님 사랑해
             | expr 아 expr 먹어라??
             | expr 아 (expr, ...?) 먹어라??
             | (id: type, ...?) 은 expr 다 게이조이고
             | () 은 expr 다 게이조이고
             | expr 게 그런 사람이 expr 일 순 없는지
             | expr 게 그런 사람이 expr 힐 순 없는지
             | 개입
rec-def    ::= id[type-var, ...]? 하는 플레이보이 예전에 (
                   visibility id: type
               ) 하더놈 같은데 이제 {
                   class-impl 
               } 하나 보지 돈 좀 버냐?? 개노잼 노라라??
             | 
unary-lop  ::= -
unary-rop  ::= 꼽표~~
binary-op  ::= expr 배 expr
             | expr 코 expr
             | expr 조이고 expr
             | expr 법회 expr
             | expr 코가커요 expr
             | expr 돈 expr 원에??
             | expr 돈 expr 원에=??
```

### Abstract Syntax

```text
identifier x
index      i
number     n
boolean    b   ::= true | false
unit       u   ::= ()
typename   tx
type-var   a
type       tau ::= tx[tau*]
                 | a
                 | tau* -> tau
                 | 유리계수
                 | (불리언)
                 | 스킵
mut        mu  ::= (mut)
case       c   ::= case x(x*) -> e
rec-def    d   ::= def x[a*]((x: tau)*): tau = e
                 | struct x[a*]((x: tau)*)
expr       e   ::= x[tau*]
                 | n
                 | b
                 | u
                 | e bop e
                 | e; e
                 | if e e e
                 | while e e
                 | let mu? x(:tau)? = e in e
                 | d in e
                 | \(x(:tau)?)*.e
                 | x := e
                 | e(e*)
                 | e match c*
```