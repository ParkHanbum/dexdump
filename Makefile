
srcdir := $(CURDIR)
objdir := $(CURDIR)

generate_operator_out := $(srcdir)/tools/generate_operator_out.py

DEXDUMP_SRCS := $(srcdir)/dexdump_cfg.cc $(srcdir)/dexdump_main.cc
DEXDUMP_SRCS += $(srcdir)/dexdump.cc
DEXDUMP_OBJS :=  $(patsubst $(srcdir)/%.cc,$(objdir)/%.op,$(DEXDUMP_SRCS))

LIBNATIVEHELPER_BASE := $(srcdir)/libnativehelper

LIBDEX_BASE := $(srcdir)/libdexfile
LIBDEX_OBJS := $(srcdir)/libdexfile/libdexfile.op

LIBARTBASE_BASE := $(srcdir)/libartbase
LIBARTBASE_OBJS := $(objdir)/libartbase/libartbase.op

LIBARTPALETTE_BASE := $(srcdir)/libartpalette
LIBZIPARCHIVE_BASE := $(srcdir)/libziparchive

LIBBASE_BASE := $(srcdir)/base
LIBBASE_HDRS := $(LIBBASE_BASE)/include
LIBBASE_OBJS := $(LIBBASE_BASE)/libbase.op

LIBLOG_BASE := $(srcdir)/liblog
LIBLOG_OBJS := $(LIBLOG_BASE)/liblog.op

ANDROID_HDRS := $(srcdir)/base/include
LIBARTPALETTE_HDRS := $(LIBARTPALETTE_BASE)/include
LIBZIPARCHIVE_HDRS := $(LIBZIPARCHIVE_BASE)/include
JNI_HDRS := $(LIBNATIVEHELPER_BASE)/include_jni

LIBCUTILS_BASE := $(srcdir)/libcutils
LIBCUTILS_HDRS := $(LIBCUTILS_BASE)/include

LIBUTILS_BASE := $(srcdir)/libutils
LIBUTILS_HDRS := $(LIBUTILS_BASE)/include

LIBSYSTEM_BASE := $(srcdir)/libsystem
LIBSYSTEM_HDRS := $(LIBSYSTEM_BASE)/include

LIBC_BASE := $(srcdir)/libc
LIBC_HDRS := $(LIBC_BASE)/include

LIBZIP_BASE := $(srcdir)/libziparchive
LIBZIP_HDRS := $(LIBZIP_BASE)/include
LIBZIP_OBJS := $(LIBZIP_BASE)/libziparchive.op

LIBLOG_BASE := $(srcdir)/liblog
LIBLOG_HDRS := $(LIBLOG_BASE)/include
LIBLOG_OBJS := $(LIBLOG_BASE)/liblog.op


CXX := clang
CXXFLAGS := -std=gnu++17 -fPIC
CXXFLAGS += -DART_STACK_OVERFLOW_GAP_arm=8192 -DART_STACK_OVERFLOW_GAP_arm64=8192 -DART_STACK_OVERFLOW_GAP_mips=16384
CXXFLAGS += -DART_STACK_OVERFLOW_GAP_mips64=16384 -DART_STACK_OVERFLOW_GAP_x86=16384 -DART_STACK_OVERFLOW_GAP_x86_64=20480
CXXFLAGS += -DART_FRAME_SIZE_LIMIT=1736
CXXFLAGS += -D__linux__

LDFLAGS := -lstdc++ -lz -lpthread -ldl

export srcdir objdir CXX CXXFLAGS generate_operator_out
export ANDROID_HDRS LIBZIPARCHIVE_HDRS LIBARTPALETTE_HDRS JNI_HDRS
export LIBCUTILS_HDRS LIBUTILS_HDRS LIBUTILS_HDRS LIBSYSTEM_HDRS
export LIBC_HDRS LIBLOG_HDRS LIBCUTILS_HDRS LIBUTILS_HDRS
export LIBZIPARCHIVE_BASE LIBARTBASE_BASE 


all: dexdump

$(LIBLOG_OBJS):
	@$(MAKE) -C $(LIBLOG_BASE)

$(LIBZIP_OBJS):
	@$(MAKE) -C $(LIBZIP_BASE)

$(LIBBASE_OBJS):
	@$(MAKE) -C $(LIBBASE_BASE)

$(LIBDEX_OBJS):
	@$(MAKE) -C $(srcdir)/libdexfile

$(LIBARTBASE_OBJS):
	@$(MAKE) -C $(srcdir)/libartbase

$(objdir)/%.op : $(srcdir)/%.cc
	$(CXX) $(CXXFLAGS) -I$(LIBBASE_HDRS) -I$(JNI_HDRS) -I$(LIBARTBASE_BASE) -I$(LIBDEX_BASE) -I$(ANDROID_HDRS) -c -o $@ $<

dexdump: $(DEXDUMP_OBJS) $(LIBLOG_OBJS) $(LIBZIP_OBJS) $(LIBBASE_OBJS) $(LIBARTBASE_OBJS) $(LIBDEX_OBJS)
	$(CXX) -o $@ $(LDFLAGS) $^

clean:
	rm -f dexdump
	rm -f $(srcdir)/*.op
	@$(MAKE) -sC $(LIBBASE_BASE) clean
	@$(MAKE) -sC $(LIBZIP_BASE) clean
	@$(MAKE) -sC $(LIBARTBASE_BASE) clean
	@$(MAKE) -sC $(LIBDEX_BASE) clean
	@$(MAKE) -sC $(LIBLOG_BASE) clean


