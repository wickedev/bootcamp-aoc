# 부트캠프 3일차

## 시행착오
- 처음에는 repeatedly-index를 만들어 전체 fabric을 만들어으나 성능 문제로 for를 사용하여 id가 있는 좌표만 생성하여 해결

## TIL
- 값이 비어있을 경우 next는 nil을 rest는 비어있는 컬랙션을 반환

## 피드백
- require에서 str 보다 string (이름 겹침 방지)
- filter 대신 keep도 유용
- 구조 분해를 사용하는 편이 깔끔
- 경우의 수를 찾을 때 math.combinatorics
- 스레딩 매크로에서 괄호 생략 가능한 경우 괄호 생략
- 컬랙션이 비어있는지 확인하기 위해 seq를 관용적으로 사용. ex: (seq []) ;; => nil

[링크](https://github.com/wickedev/bootcamp-aoc/pull/3)