---
name: gotest
description: Write Go integration/unit tests following existing patterns in the codebase. Use when the user asks to write, add, or create tests for Go code.
---

# Write Go Tests

Write tests for the specified Go functionality, strictly following existing patterns in the codebase.

## Instructions

### Phase 1: Understand existing patterns
1. Find existing tests in the same package or nearby packages:
   ```shell
   find . -name '*_test.go' -path '*/$(dirname <target>)/*' | head -10
   ```
2. Read 1-2 existing test files to understand:
   - Mock setup approach (gomock controllers, mock packages)
   - Test helper usage
   - Table-driven test structure
   - Error assertion patterns

### Phase 2: Write tests
1. **NEVER** create a new `go.mod` — use the existing one.
2. **NEVER** generate mocks — they should already exist in associated `**/mock` packages.
3. Follow these conventions:
   - Use table-driven tests with `testCases` as the slice name and `test` as the loop variable
   - Use `unit.Error` / `unit.NoError` from `common/unit/` for error assertions
   - Use `require.ErrorAssertionFunc` as the type in the test case struct
   - Use `ctx := t.Context()` when a context is needed
   - Use `assert.Equal` for comparing expected and actual results
   - Pass `testing.T` to all helper functions including setup functions
   - Check for errors before asserting on results
4. Cover both success and error cases.

### Phase 3: Verify
1. Run `go build ./...` from the module root to verify compilation.
2. Run `go test -run <TestName> ./path/to/package` to verify tests pass.
3. If tests fail, fix and re-run — do not leave broken tests.

## Example structure
```go
testCases := []struct {
    name    string
    // inputs...
    want    ResultType
    wantErr require.ErrorAssertionFunc
}{
    {
        name:    "success case",
        want:    expectedResult,
        wantErr: unit.NoError,
    },
    {
        name:    "error case",
        want:    ResultType{},
        wantErr: unit.Error,
    },
}

for _, test := range testCases {
    t.Run(test.name, func(t *testing.T) {
        result, err := functionUnderTest(test.input)
        test.wantErr(t, err, "error message")

        if err != nil {
            return
        }

        assert.Equal(t, test.want, result)
    })
}
```
