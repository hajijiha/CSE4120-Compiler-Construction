# CSE4120 · 기초컴파일러구성

Mini-C 컴파일러의 타입 검사, 중간 표현 생성, 최적화 단계를 F#으로 구현한 프로젝트 모음입니다.

| 항목 | 내용 |
|---|---|
| 학교 | 서강대학교 |
| 학기 | 2025-2 |
| 과목코드 | CSE4120 |
| 개발 환경 | F# / .NET 8 / FsLexYacc |
| 공통 자료 | [강의계획서](docs/syllabus.pdf) |

## 프로젝트

| 순서 | 프로젝트 | 구현 내용 |
|---|---|---|
| [prj1](prj1/README.md) | Type Checking | Mini-C AST의 타입 오류 검사 |
| [prj2](prj2/README.md) | IR Generation | AST를 중간 표현으로 변환 |
| [prj3](prj3/README.md) | IR Optimization | 데이터 흐름 분석과 중간 표현 최적화 |

## 저장소 구조

```text
CSE4120-Compiler-Construction/
├── README.md
├── docs/syllabus.pdf
├── prj1/  # Type Checking
├── prj2/  # IR Generation
├── prj3/  # IR Optimization
```

프로젝트별 README에서 구현 파일, 실행 명령, 관련 문서와 검증 범위를 확인할 수 있습니다.
학기와 과목코드는 해당 학기의 강의계획서를 기준으로 기록했습니다.

## 자료 출처

제출본과 수업 제공 스켈레톤·테스트 도구를 함께 정리했습니다. 제공 코드와 팀 코드의
저작권 표시를 유지하고, 직접 구현한 부분은 프로젝트별 README에 구분했습니다.

[정리 시 검증 기록](docs/verification.md)
