# Project 3 · IR Optimization

`src/Translate.fs`, `src/DFA.fs`, `src/Optimize.fs`를 구현했습니다. Mem2Reg, 상수 접기·전파, LVN, 공통 부분식과 복사 전파, Reaching Definition, 사용되지 않는 코드 제거를 조합합니다.

## 구조

`src/AST.fs`는 원본 언어의 AST, `Lexer.fsl`과 `Parser.fsy`는 제공된 전처리부,
`Main.fs`는 실행 진입점입니다. IR 단계에는 제공된 `IR.fs`, `Executor.fs`와 지원 모듈이 포함됩니다.
수업 제공 스켈레톤을 기반으로 각 단계의 타입 검사·변환·최적화 모듈을 구현했습니다.

## 빌드와 실행

.NET **8 SDK**, Python 3와 NuGet 연결이 필요합니다. .NET Runtime만 설치하면 빌드할 수 없습니다.
프로젝트 디렉터리에서 다음 명령을 실행합니다.

```bash
dotnet restore Optimize.fsproj
dotnet build Optimize.fsproj -o out
python3 check.py
```

`check.py`는 제공된 `config`, `testcase/`를 사용해 빌드와 단계별 동작을 검사합니다.
과제에서 지정한 CSPRO 서버에서도 이 명령을 사용할 수 있습니다.

## 문서와 검증

- [과제 설명](docs/assignment.pdf)
- [제출 보고서](docs/report.pdf)
- 빌드 및 테스트(2026-10-08): .NET SDK가 없어 미실행입니다. 재현에는 .NET 8 SDK가 필요합니다.
