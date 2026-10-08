# CSE4120 · 기초컴파일러구성

Mini-C의 타입 검사, 중간 표현 생성, 최적화를 F#으로 구현한 컴파일러 프로젝트입니다.

| 항목 | 내용 |
|---|---|
| 학교 | 서강대학교 |
| 학기 | 2025-2 |
| 과목코드 | CSE4120 |
| 개발 환경 | F# / .NET 8 / FsLexYacc |
| 공통 자료 | [강의계획서](docs/syllabus.pdf) |

## 프로젝트

| 순서 | 프로젝트 | 구현 내용 | 과제 자료 |
|---|---|---|---|
| [prj1](prj1/README.md) | Type Checking | Mini-C AST의 타입 오류 검사 | [과제 설명](prj1/docs/assignment.pdf) |
| [prj2](prj2/README.md) | IR Generation | AST를 중간 표현으로 변환 | [과제 설명](prj2/docs/assignment.pdf) |
| [prj3](prj3/README.md) | IR Optimization | 데이터 흐름 분석과 중간 표현 최적화 | [과제 설명](prj3/docs/assignment.pdf) |

## 저장소 구조

```text
CSE4120-Compiler-Construction/
├── README.md
├── docs/syllabus.pdf
├── prj1/  # Type Checking
├── prj2/  # IR Generation
├── prj3/  # IR Optimization
```

## 자료 출처

프로젝트에는 구현 소스와 수업 제공 스켈레톤·테스트 도구가 포함됩니다.
수업 제공 코드와 도구의 출처·라이선스는 각 원본 파일의 표기를 따릅니다.

[빌드 및 테스트](docs/verification.md)
