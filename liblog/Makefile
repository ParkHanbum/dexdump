sdir := $(srcdir)/liblog
odir := $(objdir)/liblog

LIBLOG_SRCS := $(sdir)/config_read.cpp
LIBLOG_SRCS += $(sdir)/config_write.cpp
LIBLOG_SRCS += $(sdir)/log_event_list.cpp
LIBLOG_SRCS += $(sdir)/log_event_write.cpp
LIBLOG_SRCS += $(sdir)/logger_lock.cpp
LIBLOG_SRCS += $(sdir)/logger_name.cpp
LIBLOG_SRCS += $(sdir)/logger_read.cpp
LIBLOG_SRCS += $(sdir)/logger_write.cpp
LIBLOG_SRCS += $(sdir)/logprint.cpp
LIBLOG_SRCS += $(sdir)/stderr_write.cpp
LIBLOG_SRCS += $(sdir)/fake_log_device.cpp
LIBLOG_SRCS += $(sdir)/fake_writer.cpp

LIBLOG_OBJS := $(patsubst $(sdir)/%.cpp,$(odir)/%.op,$(LIBLOG_SRCS))

_CXXFLAGS := $(CXXFLAGS) -DFAKE_LOG_DEVICE=1
_CXXFLAGS += -I$(sdir)/include -I$(LIBCUTILS_HDRS)

all: $(odir)/liblog.op $(LIBLOG_OBJS)

$(odir)/%.op: $(sdir)/%.cpp
	$(CXX) $(_CXXFLAGS) -c $< -o $@ 

$(odir)/liblog.op: $(LIBLOG_OBJS)
	$(LD) -r -o $@ $^

clean:
	rm -f $(odir)/*.op
