# DataScience_ITS_EATs

데이터 사이언스 프로젝트 

# checklist

PPT 예시로... 마케팅과 어떻게 보여줄지?

1. 새로운 데이터로 구별로도 돌려보고..
2. 클러스터 하고.
3. 시각화 아이디어(한사람당 3개씩)
4. Xaringan
5. MODEL 나눠서 돌릴수 있나.

### 데이터 줄이기
- 추가 변수 뭘 합쳐서 데이터 만들었는지.
- 샘플링 하는거 날짜 반으로 쪼갤것인가. 구별로 쪼갤 것인가
- 행정동 나누는거(clustering?)
- 구별로 따로 분석하여 연관성을 버릴 것인가.
- 치킨, 피자, 중국집을 나워서 모델링 할 것인가.
- 연령대별 시각화(우리가 왜 50~60대를 묶게 되었는지를 보여주는) / 연령대별 묶은 데이터 / 연령대 별로 묶기.
   연령별로 합쳤다.
- 파생변수?
   - 스포츠 경기
   - 네이버 데이터랩 API

### 데이터 합치기
- 동묶는거
- 날씨 서울시 평균으로 처리
- 결측치 처리
- 1인가구

### 모델링
- 머신러닝(Glm, Gam, Rf, Nnet)
- spatio temporal
- hierarchy
- 시계열 (2018과 2019 비슷한가?)

### 시각화
- 교수님 깃헙 (ggPlot, tidyverse)

### 결과 분석
- 의미 분석

### 서비스 제안
- ppt (자린간 예쁘게 만들기)
- 보고서
- 개인용 = ex. 비오는날 도곡동의 10대 남자에게 개인 추천.
             type만 치킨, 중국집, 피자으로 다른 세개의 데이터를 모델에 돌려서 얻은 콜수 세가지.
             => 비율을 확률이라고 추정하여 몇퍼센트 개인 추천.
- 기업용 = ex. 비오는날 도곡동 중국집이 예상할수있는 콜수 추천.
             개인정보 (성별, 연령)만 다른 여러개의 x 데이터를 만들어서 얻은 콜수 합.

5/18 데사입 조모임

1. 시각화 (5/18)
위도 경도 이외에 shape 타일 찾아서 가져다 붙여야 함
2. 전체 데이터가 안 돌아간다고 했을 때 
2-1. sampling  
2-2. 구별로 따로 분석하기. (구별 연관성 무시)
2-3. 치킨/피자/중국집 나눠서 돌리기. 3개 모델. 
2-4. 연령대별로 묶기: 10대/20대/ 30~40대/50~60대 
3.. 모델: 

1. 연령대별 시각화(우리가 왜 50~60대를 묶게 되었는지를 보여주는)
2. 연령대별 묶은 데이터
-----------------------------------------------------------------------------------------------------------------
5/26 조모임

1. 파생변수를 더 추가?: 스포츠경기(프로야구)있는 날
2. 행정동 별로 어떻게 나눌 건지 
3. 시각화
4. PPT
5. 보고서

5/30까지 할일
1. 구별로 돌려보기-분석 : 회귀X, 랜포 안돌아감. 
2. k-means clustering   
3. 시각화 : 한 사람당 2~3개씩 발표에 도움될 만한 아이디어 생각해오기.
(나)
1. 행정동별 중국집/치킨/피자 배달량 분포
-행정동별 상이한 결과를 나타내는 구를 중심으로

2. 요일별/ 계절별 배달량 분포 
-궁금해할 것 같음

3. 비가 내린 날/내리지 않은 날 별로
치킨/중국집/피자

4. 행정동별 주문량 순위 + 인구수 활용해서 =주문량/인구 수 표현
>>인구 수 대비 주문을 많이 한 동네를 파악할 수 있다

5. 전체 배달량 합쳐서 그 안에서 치킨+피자+중국집 비율 시각화
------------------------------------------------------------------------------------------------------------------
5/31 조모임
1. 동이 중요한 이유
-동을 분석에서 빼는 대신에 동의 특성을 넣자.
-6~8월만 하는 이유?(여름)

3. 중국집. 치킨. 피자 나눠서 돌리자
4. 행정동과 법정동 어떻게 할 것인지가. 
법정동을 행정동으로 바꾸면 (자양동>자양 1,2,3,4 동인 경우 데이터를 어떻게 분배할 지 문제가 생긴다)
onehotencoding 으로 동 개수만큼 카테고리 개수를 설정해서 분석한다. 
5. 추가할 변수: 소득 분위, 인구 수, 연령대, 
 
[추가 변수]
1. 중국집/피자/치킨 나눠서 돌리고, 6~8월만 돌리고, 구별로 나눠서 돌리고.
2. 법정동에서 행정동으로 imputation 
3. 법정동/행정동
4. 평균소득분위. 인구 수. 연령대

---------------------------------------------------------------------------------------------------------------
개인적으로 생각한 것 (4월)
-어떻게 시각화할 수 있는지 방법 생각해보기
1. 서울시 지도에서 통화량 분포가 어떻게 되는지 살펴보기: 상완오빠
2. 요일별 흐름
3. 계절성을 띄는지 시계열 데이터로 확인하기-계절성을 띈다면?
4. 행정동 인구 수 파악하기
1) 연령대별: https://data.seoul.go.kr/dataList/datasetView.do?infId=423&srvType=S&serviceKind=2&currentPageNo=1&searchValue=&searchKey=null
2) 행정동별: https://data.seoul.go.kr/dataList/datasetView.do?infId=10043&srvType=S&serviceKind=2&currentPageNo=1&searchValue=&searchKey=null

5. 서울시 차량통행속도(구별/월별)통계: 2017년
https://data.seoul.go.kr/dataList/datasetView.do?infId=10846&srvType=S&serviceKind=2&currentPageNo=1&searchValue=&searchKey=null
5. 음식점 개수 파악하기 
6. 서울소비경기지수: https://www.si.re.kr/node/61153
(도심권, 동남권, 동북권, 서남권, 서북권)

-어떤 식으로 분석할 수 있을지
-시각화 자료는 어떤 점을 할 수 있을 지
-프로젝트 진행 방향(분석/ 예측)
-지방행정 인허가 데이터개방: https://www.localdata.kr/main.do

--------------------------------------------------------------------------------------------------------------------
6/4

review: 소득분위 데이터 찾았음. 

1) 법정동vs행정동
-법정동 그대로 두고 행정동으로 설명된 변수를 법정동에 맞게 붙인다
-데이터의 법정동을 행정동으로 바꾼다
-법정동 그래도 두고 행정동 변수 추가하지 않는다.

2) 분석 모델
-
-
3) 시각화 
-
-
4) 보고서 
-
-
5) ppt
-
-
--------------------------------------------------------------------------------------------------------------------
[프로젝트 발표 방향]
