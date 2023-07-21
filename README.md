# MTELang

### MTELang, the Machine to Titan-nose Expression Language

Scala 3 내부에서 돌릴 수 있는 작은 프로그래밍 언어예요~ 

### 예제

#### 구구단 출력하기

```scala
import mte._
import scala.language.postfixOps

def 구구단(): Unit = {
  춘잣! {
    (11수) ("i") {
      (11수) ("j") {
        "i" 조이고 "j" 리액션 "%s\t"
      } 리액션 "\n"
    }
  }
}
```

#### 백준 23803. [ㄴ 찍기](https://www.acmicpc.net/problem/23803)

안타깝게도 백준은 스칼라 2까지만 지원하기 때문에 백준에 제출할 수는 없다 맨이야.
~~지금 몇 신데 아직도 지원을 안 해!! 아 미친 주문이 아직 안 됐네~~

```scala
import mte._
import scala.language.postfixOps

def ㄴ찍기(): Unit = {
  춘잣! {
    아니세상에 자기가 "케인" 이라는사람인데 개입 을 했대
  } 케바바바밥줘 {
    ("케인" 조이고 4수) ("i") {
      ("케인"수) ("j") {
        리액션 (스키비야, "@")
      } 리액션 "\n"
    }
  } 케바바바밥줘 {
    ("케인"수) ("i") {
      ("케인" 조이고 5수) ("j") {
        리액션 (스키비야, "@")
      } 리액션 "\n"
    }
  }
}
```

#### 팩토리얼 계산하기 (재귀함수)

```scala
import mte._
import scala.language.postfixOps

def 팩토리얼(): Unit = {
  춘잣! {
    아~! ("팩토리얼", "x")는 {
      (유링게슝한?) ("x" 돈 탱 원에??) {
        "팩토리얼"아 ("x" 코 뭉) 먹어라?? 조이고 "x"
      } 안유링게슝 {
        뭉
      }
    } 가 참 좋구나~!
  } 케바바바밥줘 {
    (11수) ("영양제") {
      "팩토리얼"아 "영양제" 먹어라?? 리액션 "%s\n"
    }
  }
}
```

### 문법

