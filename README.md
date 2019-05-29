# Datamining_project
- [2019-04-24] 각자 데이터 살펴보고 간단한 모델링 해보기 (소현 - 항공 데이터 / 승민 - 산불 / 은진 - 음쓰)
- [2019-04-26] 승민 : 멜론 연간 차트 TOP100어때!! / 은진 : 음쓰 안 해!! / 소현 : XML 파싱 ㅜㅜ 성공!!!!!!!!!!
- [2019-04-27] 내가 왜 크롤링해온다고 했을까...ㅎㅎ 귀찮어 ㅜㅜ 눈아파ㅜㅜ
- [2019-04-28] concat 왜 안되는지 의문 ㅎ
- [2019-05-01] music 뺏겼고,, 항공은 실시간 데이터 뿐이고,, 웹툰은 까였고,, 남은 건 산불뿐,,
- [2019-05-03] 주제 선정 완료!! 데이터 출처 [link](http://topis.seoul.go.kr/refRoom/openRefRoom_1_2.do) 시간대별, 도로별 속도 예측
- [2019-05-04] 2019-03 만 해도 350만갠데 1년치 다 못 할 것은데,,
- [2019-05-07] 은진 - 날씨 준비해올 것(날씨요정) / 소현 - 2018.12월 데이터로 EDA / 승민 - 2019.03월 데이터로 EDA
- [2019-05-10] 도로별 공간정보를 담을 수 있는 것들 좀 찾아올것
- [2019-05-20] 도로별 정보 요청해보기! 없으면 서울시 차량통행속도 보고서에서 일일히 입력! 161개씩, 통행량은 좀 더 생각해보기! **날씨 공모전 접수!!! 오늘 서울은 하루종일 맑음**
- [2019-05-25] 날씨는 데이터가 너무 구려서 접음. 도로 변수들 다 채워넣었다! 이제 분석만 하면 
- [2019-05-27] 날씨 결측치 채웠고, 교통사고 데이터 은지니가 일일히 다운~ (칭찬해)
- [2019-05-28] 날씨 다 합쳤다!


### 어떻게 하지?
- 날씨 데이터 시간(0시~23시) / 교통 데이터 시간(1시~24시) -> 0시 = 1시 ... 23시 = 24시

### 파생변수
- 공휴일, 휴일
- 연속적으로 쉬는 날
- 도로별 사고 건수(count)
- 사망자 수, 중상자 수, 경상자 수, 부상신고자 수 각각
- 사망자 수, 중상자 수, 경상자 수, 부상신고자 수 합
- 사고 심각도
- 위의 값들을 나누는 기준을 도로 길이 or 구간 수 or 진출입 수



### 인사이트
- 모델은 나눠서 할 것
- 1~5시는 뺄 것 (대중교통이 없고, 막히지 않음)
- 링크 아이디

  2018-04 ~ 2019-03
  - 날씨 (결측치는 해당날짜와 시간의 평균값으로 대체)
  - 교통사고 
  - 행사
  - 차 없는 거리
  - 미세먼지
  
### 날씨 데이터 단위 정리
- temperature : 기온(섭씨)
- wind : 풍속(m/s)
- rain : 강수량(mm)
- snow : 적설량(cm)



## 다음 미팅은 [2019-05-27]

