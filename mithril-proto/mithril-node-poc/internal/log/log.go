package log

import (
	"fmt"
	"github.com/pkg/errors"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
	"strings"
	"time"
)

// Level is log level for any logger
type Level zapcore.Level

// Duplicated constants from zap for more intuitive usage
const (
	// DebugLevel logs are typically voluminous, and are usually disabled in
	// production.
	DebugLevel = Level(zap.DebugLevel)
	// InfoLevel is the default logging priority.
	InfoLevel = Level(zap.InfoLevel)
	// WarnLevel logs are more important than Info, but don't need individual
	// human review.
	WarnLevel = Level(zap.WarnLevel)
	// ErrorLevel logs are high-priority. If an application is running smoothly,
	// it shouldn't generate any error-level logs.
	ErrorLevel = Level(zap.ErrorLevel)
	// DPanicLevel logs are particularly important errors. In development the
	// logger panics after writing the message.
	DPanicLevel = Level(zap.DPanicLevel)
	// PanicLevel logs a message, then panics.
	PanicLevel = Level(zap.PanicLevel)
	// FatalLevel logs a message, then calls os.Exit(1).
	FatalLevel = Level(zap.FatalLevel)
)

var l *zap.Logger

var log *zap.SugaredLogger

var logLevel *zap.AtomicLevel

// SetLevel sets level
func SetLevel(level Level) {
	getDefaultLoggerOrPanic() // init logger if it hasn't yet been
	logLevel.SetLevel(zapcore.Level(level))
}

// SetLevelStr sets level of default logger from level name
// Valid values: debug, info, warn, error, dpanic, panic, fatal
func SetLevelStr(levelStr string) {
	l := getDefaultLoggerOrPanic() // init logger if it hasn't yet been
	err := logLevel.UnmarshalText([]byte(levelStr))
	if err != nil {
		l.Error("can't change log level: invalid string value provided")
		return
	}
}

func getDefaultLoggerOrPanic() *zap.SugaredLogger {
	var err error
	if log != nil {
		return log
	}
	// default level: debug
	l, logLevel, err = NewLogger("debug", []string{"stdout"})
	if err != nil {
		panic(err)
	}
	log = l.Sugar()
	return log
}

// NewLogger creates the logger with defined level. outputs defines the outputs where the
// logs will be sent. By default, outputs contains "stdout", which prints the
// logs at the output of the process. To add a log file as output, the path
// should be added at the outputs array. To avoid printing the logs but storing
// them on a file, can use []string{"pathtofile.log"}
func NewLogger(levelStr string, outputs []string) (*zap.Logger, *zap.AtomicLevel, error) {
	var level zap.AtomicLevel
	err := level.UnmarshalText([]byte(levelStr))
	if err != nil {
		return nil, nil, fmt.Errorf("error on setting log level: %s", err)
	}

	cfg := zap.Config{
		Level:            level,
		Encoding:         "console",
		OutputPaths:      outputs,
		ErrorOutputPaths: []string{"stderr"},
		EncoderConfig: zapcore.EncoderConfig{
			MessageKey:  "message",
			LevelKey:    "level",
			EncodeLevel: zapcore.CapitalColorLevelEncoder,
			TimeKey:     "timestamp",
			EncodeTime: func(ts time.Time, encoder zapcore.PrimitiveArrayEncoder) {
				encoder.AppendString(ts.Local().Format(time.RFC3339))
			},
			EncodeDuration: zapcore.SecondsDurationEncoder,
			CallerKey:      "caller",
			EncodeCaller:   zapcore.ShortCallerEncoder,

			// StacktraceKey: "stacktrace",
			StacktraceKey: "",
			LineEnding:    zapcore.DefaultLineEnding,
		},
	}
	logger, err := cfg.Build()
	if err != nil {
		return nil, nil, err
	}
	defer logger.Sync()
	l := logger.WithOptions(zap.WithCaller(false))
	return l, &level, nil
}

// Debug calls log.Debug
func Debug(args ...interface{}) {
	getDefaultLoggerOrPanic().Debug(args...)
}

// Info calls log.Info
func Info(args ...interface{}) {
	getDefaultLoggerOrPanic().Info(args...)
}

// Warn calls log.Warn
func Warn(args ...interface{}) {
	args = appendStackTraceMaybeArgs(args)
	getDefaultLoggerOrPanic().Warn(args...)
}

// Panic calls log.Panic
func Panic(args ...interface{}) {
	args = appendStackTraceMaybeArgs(args)
	getDefaultLoggerOrPanic().Panic(args...)
}

// Error calls log.Error
func Error(args ...interface{}) {
	args = appendStackTraceMaybeArgs(args)
	getDefaultLoggerOrPanic().Error(args...)
}

// Fatal calls log.Fatal
func Fatal(args ...interface{}) {
	args = appendStackTraceMaybeArgs(args)
	getDefaultLoggerOrPanic().Fatal(args...)
}

// Debugf calls log.Debugf
func Debugf(template string, args ...interface{}) {
	getDefaultLoggerOrPanic().Debugf(template, args...)
}

// Infof calls log.Infof
func Infof(template string, args ...interface{}) {
	getDefaultLoggerOrPanic().Infof(template, args...)
}

// Warnf calls log.Warnf
func Warnf(template string, args ...interface{}) {
	getDefaultLoggerOrPanic().Warnf(template, args...)
}

// Fatalf calls log.Warnf
func Fatalf(template string, args ...interface{}) {
	getDefaultLoggerOrPanic().Fatalf(template, args...)
}

// Errorf calls log.Errorf and stores the error message into the ErrorFile
func Errorf(template string, args ...interface{}) {
	getDefaultLoggerOrPanic().Errorf(template, args...)
}

// Debugw calls log.Debugw
func Debugw(template string, kv ...interface{}) {
	getDefaultLoggerOrPanic().Debugw(template, kv...)
}

// Infow calls log.Infow
func Infow(template string, kv ...interface{}) {
	getDefaultLoggerOrPanic().Infow(template, kv...)
}

// Warnw calls log.Warnw
func Warnw(template string, kv ...interface{}) {
	template = appendStackTraceMaybeKV(template, kv)
	getDefaultLoggerOrPanic().Warnw(template, kv...)
}

// Errorw calls log.Errorw
func Errorw(template string, kv ...interface{}) {
	template = appendStackTraceMaybeKV(template, kv)
	getDefaultLoggerOrPanic().Errorw(template, kv...)
}

// Fatalw calls log.Fatalw
func Fatalw(template string, kv ...interface{}) {
	template = appendStackTraceMaybeKV(template, kv)
	getDefaultLoggerOrPanic().Fatalw(template, kv...)
}

// appendStackTraceMaybeArgs will append the stacktrace to the args if one of them
// is an Error
func appendStackTraceMaybeArgs(args []interface{}) []interface{} {
	for i := range args {
		if err, ok := args[i].(causer); ok {
			cause := causeWithStackTrace(err.(error))
			if stErr, ok := cause.(stackTracer); ok {
				st := stErr.StackTrace()
				for i := 0; i < len(st)-2; i++ {
					args = append(args, "\n", fmt.Sprintf("%+v", st[i]))
				}
				return args
			}
			return append(args, fmt.Sprintf("%+v", cause))
		}
	}
	return args
}

// appendStackTraceMaybeKV will append the stacktrace to the KV if one of them
// is an Error
func appendStackTraceMaybeKV(msg string, kv []interface{}) string {
	for i := range kv {
		if i%2 == 0 {
			continue
		}
		// print stack trace here
	}
	return msg
}

func causeWithStackTrace(err error) error {
	for err != nil {
		errCauser, ok := err.(causer)
		if !ok {
			break
		}
		cause := errCauser.Cause()
		_, ok = cause.(stackTracer)
		if !ok {
			break
		}
		err = cause
	}
	return err
}

// funcName removes the path prefix component of a function's name reported by func.Name().
func funcName(name string) string {
	i := strings.LastIndex(name, "/")
	return name[i+1:]
}

type stackTracer interface {
	StackTrace() errors.StackTrace
}

type causer interface {
	Cause() error
}
