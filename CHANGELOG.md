# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.1.1 - 2021-07-01

### Fixed

- Close frame codes and reasons are now nillable instead of defaulted
  - The WebSocket spec does not require that a code and reason be included
    for all close frames

## 0.1.0 - 2021-06-30

### Added

- Initial implementation
    - includes HTTP/1.1 and HTTP/2 support and extensions
