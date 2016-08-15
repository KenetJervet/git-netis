Git Netis
===

A Netis internal Git utility that integrates JIRA (and possibly confluence in the future)

## Synopsis

This tool assumes your git workflow follows the Netis git flow variation. See [https://git.dev.netis.com/projects/RD/repos/netisgitflow](https://git.dev.netis.com/projects/RD/repos/netisgitflow) for details.

## Usage

### Prepare for work

```bash
$ git netis setup -i ⏎

Who are you? kenneth.zhao ⏎
Password? ****** ⏎

BPC:
[1] barplayback
[2] bpc-doc
[3] bpc-loadtest
[4] bpc-t
[5] bpc-x
...

CKIT:
no visible repos

JKIT:
[6] cap-toolkit
...

Select a Bitbucket repo (Ctrl-C to exit): 5 ⏎

[1] Bohr
[2] Confirmed Defect
[3] DataAce
...

Select a JIRA project (Ctrl-C to exit): 1 ⏎

You are now working on repo `bpc-x` for project `Bohr`.

$
```

### Find issues

Find your issues that are yet to be done:
```bash
$ git netis query todo

[1] BOHR-3076 [BUG] 查询字符串和指标、分组处仍然显示value而不显示text。应该显示为text。

$
```

### Work on issue

```bash
$ git netis workon -i ⏎

[1] BOHR-3076 [BUG] 查询字符串和指标、分组处仍然显示value而不显示text。应该显示为text。
[2] BOHR-3034 [BUG] ...

Select an issue to workon: 1 ⏎
...
```

OR

```bash
(bugfix/3076) $ git netis workon 3076 ⏎

Work on BOHR-3076. Proceed? y ⏎
Now you can start working on BOHR-3076.

(bugfix/3076) $
```

### Claim an issue done

```bash
(bugfix/3076) $ git netis done ⏎

Done working on BOHR-3076. Proceed? y ⏎
You have done working on BOHR-3076. Want to create a code review? y ⏎
# Opens a browser and goes to the page that guides you through creating a code review

(bugfix/3076) $
```
