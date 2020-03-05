PAGESIZE := $(shell getconf PAGE_SIZE)

sdir := $(srcdir)/base
odir := $(objdir)/base

LIBBASE_SRCS := $(sdir)/chrono_utils.cpp
LIBBASE_SRCS += $(sdir)/cmsg.cpp
LIBBASE_SRCS += $(sdir)/file.cpp
LIBBASE_SRCS += $(sdir)/logging.cpp
LIBBASE_SRCS += $(sdir)/mapped_file.cpp
LIBBASE_SRCS += $(sdir)/parsenetaddress.cpp
LIBBASE_SRCS += $(sdir)/properties.cpp
LIBBASE_SRCS += $(sdir)/quick_exit.cpp
LIBBASE_SRCS += $(sdir)/stringprintf.cpp
LIBBASE_SRCS += $(sdir)/strings.cpp
LIBBASE_SRCS += $(sdir)/threads.cpp
LIBBASE_SRCS += $(sdir)/test_utils.cpp
LIBBASE_SRCS += $(sdir)/errors_unix.cpp

LIBBASE_OBJS := $(patsubst $(sdir)/%.cpp,$(odir)/%.op,$(LIBBASE_SRCS))

_CXXFLAGS := $(CXXFLAGS) -Wall -Wextra -D_FILE_OFFSET_BITS=64
_CXXFLAGS += -Wexit-time-destructors
_CXXFLAGS += -I$(ANDROID_HDRS) 
_CXXFLAGS += -DPAGE_SIZE=$(PAGESIZE)

all: $(odir)/libbase.op $(LIBBASE_OBJS)

$(odir)/%.op: $(sdir)/%.cpp
	$(CXX) $(_CXXFLAGS) -c $< -o $@ 

$(odir)/libbase.op: $(LIBBASE_OBJS)
	$(LD) -r -o $@ $^

clean:
	rm -f $(odir)/*.op
