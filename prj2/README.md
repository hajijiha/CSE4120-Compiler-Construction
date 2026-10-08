# Project 2 · IR Generation

`src/Translate.fs`에서 Mini-C의 AST를 IR 명령으로 변환합니다. 배열과 포인터 연산, 주소 계산, 조건 분기, 반복문과 단락 평가를 처리합니다.

## 구조와 출처

`src/AST.fs`는 원본 언어의 AST, `Lexer.fsl`과 `Parser.fsy`는 제공된 전처리부,
`Main.fs`는 실행 진입점입니다. IR 단계에는 제공된 `IR.fs`, `Executor.fs`와 지원 모듈이 포함됩니다.
해당 단계의 수업 스켈레톤에 별도로 보관된 구현 최종본을 적용했습니다.

## 빌드와 실행

.NET **8 SDK**, Python 3와 NuGet 연결이 필요합니다. .NET Runtime만 설치하면 빌드할 수 없습니다.
프로젝트 디렉터리에서 다음 명령을 실행합니다.

```bash
dotnet restore IRGenerate.fsproj
dotnet build IRGenerate.fsproj -o out
python3 check.py
```

`check.py`는 제공된 `config`, `testcase/`를 사용해 빌드와 단계별 동작을 검사합니다.
과제에서 지정한 CSPRO 서버에서도 이 명령을 사용할 수 있습니다.

## 문서와 검증

- [과제 설명](docs/assignment.pdf)
- [제출 보고서](docs/report.pdf)
- 제출 보고서에는 구현 과정과 수업에서 요구한 도구 사용 내역이 포함돼 있습니다.
- 2026-10-08 정리 환경에는 .NET SDK가 없어 빌드와 제공 테스트를 새로 실행하지 않았습니다.
